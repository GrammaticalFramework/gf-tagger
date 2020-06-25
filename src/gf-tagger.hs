{-# LANGUAGE BangPatterns, RankNTypes #-}
import PGF2
import Data.Maybe
import Data.Either
import Data.List
import Data.Char
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import qualified Data.Vector as V
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Storable.Mutable as MV
import Control.Monad
import HagerZhang05
import System.Random
import System.Random.Shuffle
import System.Environment
import GHC.Float

import Context
import Features

main = do
  args <- getArgs
  case args of
    (gr_path:lang:args) -> do putStrLn ("Loading "++gr_path)
                              gr <- readPGF gr_path
                              let Just cnc = Map.lookup lang (languages gr)
                              case args of
                                ["-train",ex_path,out_path]
                                   -> doTraining gr cnc ex_path out_path
                                ["-test", ex_path]
                                   -> doTesting gr cnc ex_path
                                ["-tag",model_path,inp_path,out_path]
                                   -> doTagging gr cnc model_path inp_path out_path
                                ["-tokenize",inp_path,out_path]
                                   -> doTokenize gr cnc inp_path out_path
                                _  -> doHelp
    _                              -> doHelp

doTraining gr cnc corpus_path out_path = do
  es <- readCorpus corpus_path
  putStrLn ("Read "++show (length es)++" sentences")
  let hcounts0     = emptyIndex
      (tcounts0,_) = getLexicalTags gr cnc
      fvalues0     = emptyIndex
      contexts     = map (getContexts gr cnc) es
  model <- trainModel hcounts0 tcounts0 fvalues0 contexts
  putStrLn ("Model saved in "++out_path)
  writeModel out_path model

doTesting gr cnc corpus_path = do
  es <- readCorpus corpus_path
  let es_len = length es
  putStrLn ("Read "++show es_len++" sentences")
  g <- newStdGen
  let hcounts0     = emptyIndex
      (tcounts0,_) = getLexicalTags gr cnc
      fvalues0     = emptyIndex
      all_contexts = map (getContexts gr cnc) es
      len          = es_len `div` 10
      partition    = chunk (shuffle' all_contexts es_len g) [] es_len len
  as <- cross_validate hcounts0 tcounts0 fvalues0 1 partition
  print as
  print (sum as / fromIntegral (length as))
  where
    chunk xs yss l len
      | l-len < len = [(concat yss,xs)]
      | otherwise   = let (ys,zs) = splitAt len xs
                      in (concat (zs:yss),ys) :
                         chunk zs (ys:yss) (l-len) len

    cross_validate hcounts0 tcounts0 fvalues0 i []                                    = return []
    cross_validate hcounts0 tcounts0 fvalues0 i ((tr_contexts,ev_contexts):partition) = do
      putStrLn ("Evaluation on partion "++show i)
      model <- trainModel hcounts0 tcounts0 fvalues0 tr_contexts
      (c,t) <- evaluate model ev_contexts 0 0
      let a = fromIntegral c/fromIntegral t
      print (c,t,a)
      as <- cross_validate hcounts0 tcounts0 fvalues0 (i+1) partition
      return (a:as)
      where
        evaluate model []                     c t = return (c,t)
        evaluate model (contexts:ev_contexts) c t = do
          let sent = contexts2sentence "" contexts
          (c,t) <- count sent (map restoreTag (tagSentence gr cnc model sent)) contexts c t
          evaluate model ev_contexts c t

        contexts2sentence space []                       = ""
        contexts2sentence space (BIND_TAG     :contexts) =
          contexts2sentence "" contexts
        contexts2sentence space (Word w       :contexts) =
          space++w++contexts2sentence " " contexts
        contexts2sentence space (WordTag w _ _:contexts) =
          space++w++contexts2sentence " " contexts

        count sent []           []           !c !t = return (c,t)
        count sent (tag1:tags1) (tag2:tags2) !c !t
          | tag1 == tag2 = count sent tags1 tags2 (c+1) (t+1)
          | otherwise    = count sent tags1 tags2 c     (t+1)
        count sent tags1        tags2        !c !t = do
          putStrLn "Warning: missmatching sizes, for:"
          putStrLn sent
          return (c,t)

        restoreTag (i,w,[],j)                     = Word w
        restoreTag (i,w,(fn,field,fn_prob):ans,j) = WordTag w cat field
          where
            Just (_,cat,_) = fmap unType (functionType gr fn)

getLexicalTags gr cnc =
  mapAccumL addIndex emptyIndex
            [Tag cat an | (w,ans) <- fullFormLexicon cnc, (f,an,_) <- ans
                        , Just (_,cat,_) <- [fmap unType (functionType gr f)]]

getContexts :: PGF -> Concr -> Expr -> [Context]
getContexts gr cnc e = concatMap tags (bracketedLinearize cnc e)
  where
	tags (Leaf w)                = [Word w]
	tags BIND                    = [BIND_TAG]
	tags b@(Bracket cat _ an fun bs)
	  | arity fun == Just 0      = [WordTag (unwords (flattenBracketedString b)) cat an]
	  | otherwise                = concatMap tags bs

	arity fun =
	  case fmap unType (functionType gr fun) of
		Just (hs,_,_) -> Just (length hs)
		Nothing       -> Nothing

trainModel hcounts0 tcounts0 fvalues0 contexts = do
  let wordCounts   = foldl' (foldl' addWordCount) Map.empty contexts
      (cSize,hcounts,tcounts,fvalues1) =
          foldl (\st -> extractFeatures wordCounts left3words naacl2003unknowns st)
                (0,hcounts0,tcounts0,fvalues0)
                contexts
      (fSize,fvalues) = Map.mapAccumWithKey keepPopulated 0 fvalues1
      y2tag =
        V.replicate (Map.size hcounts) undefined V.// 
                    [(i, t) | (t,(i,c)) <- Map.toList tcounts]
  putStrLn ("Processed "++show (cSize-length contexts)++" words")
  putStrLn ("Found that xSize="++show (Map.size hcounts)++" and ySize="++show (Map.size tcounts))
  putStrLn ("Found "++show (Map.size fvalues)++" unique features")
  putStrLn ("Found "++show (sum (fmap (\(_,tags) -> length tags) fvalues))++" templates, "++
            "populated "++show fSize++" templates")
  let (fun,grad,comb) = maxentProblem 0.5 hcounts tcounts fvalues
  (lam,res,stat) <- optimize defaultParameters{verbose=VeryVerbose} 1e-4
                             (VS.replicate fSize 0.0)
                             fun grad (Just comb)
  return (Set.fromList [w | Word w <- Map.keys tcounts]
         ,Map.fromList [((val,y2tag V.! t),lam VS.! j) | (val,(Right hists,tags)) <- Map.toList fvalues, (j,t,c) <- tags]
         )
  where
    addWordCount counts (Word w)        = Map.insertWith (+) w 1 counts
    addWordCount counts (WordTag w _ _) = Map.insertWith (+) w 1 counts
    addWordCount counts _               = counts


doTagging gr cnc model_path corpus_path output_path = do
  putStrLn ("Reading "++model_path)
  model <- readModel model_path
  ls <- fmap lines $ readFile corpus_path
  putStrLn ("Output saved in "++output_path)
  writeFile output_path ((unlines . concat . intersperse [""] . map (map show .tagSentence gr cnc model)) ls)

tagSentence gr cnc model =
  bestSeq model left3words naacl2003unknowns [(replicate 2 START,(0,[]))] .
  map (toTags model Map.empty) .
  filterBest .
  lookupCohorts cnc
  where
    toTags model tags (i,w,[]                       ,j) =
      let context0 = Map.toList tags
          context  = if null context0 || Set.member w (fst model)
                       then (Word w,(i,w,[],j)):context0
                       else context0
      in (Word w,context)
    toTags model tags (i,w,an@(fn,field,fn_prob):ans,j) =
      let Just (_,cat,_) = fmap unType (functionType gr fn)
          tag   = Tag cat field
          tags' = case Map.lookup tag tags of
                    Nothing          ->
                      Map.insert tag (i,w,[an],j) tags
                    Just (i,w,ans,j) ->
                      Map.insert tag (i,w,an:ans,j) tags
      in toTags model tags' (i,w,ans,j)

    bestSeq model extractorsG extractorsR lefts []                     =
      head [reverse anns | (left,(p,anns)) <- lefts, p == maximum [p | (left,(p,anns)) <- lefts]]
    bestSeq model extractorsG extractorsR lefts ((wtag,context):right) =
      let lefts' =
            argMax [left'
                      | left  <- lefts
                      , left' <- shift model extractorsG extractorsR
                                       left wtag context (map fst right++repeat END)
                   ]
      in bestSeq model extractorsG extractorsR lefts' right

    shift (words,probs) extractorsG extractorsR (left,(p,anns)) wtag context right =
      norm [(comb wtag tag:init left,lambdas tag,ann:anns) | (tag,ann) <- context]
      where
        comb (Word w) (Tag cat field) = WordTag w cat field
        comb _        tag             = tag

        lambdas tag =
          sum [Map.findWithDefault 0 (extractor left wtag right,tag) probs
                           | extractor <- extractorsG]

        norm lefts = [(left,(c-s+p,anns)) | (left,c,anns) <- lefts]
          where
            m = maximum [c | (_,c,_) <- lefts]
            s = m + log (sum [exp (c-m) | (_,c,_) <- lefts])

    argMax = (Map.toList . Map.fromListWith max)
      where
        max (p1,ann1) (p2,ann2)
          | p1 > p2   = (p1,ann1)
          | otherwise = (p2,ann2)

doTokenize gr cnc corpus_path output_path = do
  ls <- fmap lines $ readFile corpus_path
  putStrLn ("Output saved in "++output_path)
  writeFile output_path ((unlines . concat . intersperse [""] . map (map show . filterBest .lookupCohorts cnc)) ls)

doHelp = do
  name <- getProgName
  putStrLn ("Synopsis:")
  putStrLn ("   "++name++"<pgf path> <concrete syntax> -train <examples path> <output path>")
  putStrLn ("   "++name++"<pgf path> <concrete syntax> -test  <examples path>")
  putStrLn ("   "++name++"<pgf path> <concrete syntax> -tag   <model path> <input path> <output path>")
  putStrLn ("")
  putStrLn ("<examples path> must contain abstract syntax trees, one per line prefixed with \"abs:\"")
  putStrLn ("<model path>    must contain the trained model")
  putStrLn ("<input path>    must contain one sentence per line")
  putStrLn ("<output path>   must point to the file to be generated")

extractFeatures wordCounts extractorsG extractorsR st =
  collect st (repeat START)
  where
    collect (!cSize,hcounts,tcounts,fvalues) left []              =
      let history      = [val | extractor <- extractorsG
                              , let val = extractor left END (repeat END)
                              , val /= None]
          tag          = END
          (hcounts',x) = addIndex hcounts history
          (tcounts',y) = addIndex tcounts tag
          fvalues'     = foldl (addValue x y) fvalues history
      in (cSize+1,hcounts',tcounts',fvalues')
    collect (!cSize,hcounts,tcounts,fvalues) left (context:right) =
      let extractors   = case wordOnly context of
                           Word w -> case Map.lookup w wordCounts of
                                       Just n | n > 5 -> extractorsG
                                       _              -> extractorsG {- ++
                                                         extractorsR -}
                           _                          -> extractorsG
          history      = [val | extractor <- extractors
                              , let val = extractor left context (right++repeat END)
                              , val /= None]
          tag          = tagOnly context
          (hcounts',x) = addIndex hcounts history
          (tcounts',y) = addIndex tcounts tag
          fvalues'     = foldl (addValue x y) fvalues history
      in collect (cSize+1,hcounts',tcounts',fvalues') (context:left) right

emptyIndex = Map.empty

addIndex tcounts x =
  let p@(!i,!c) = case Map.lookup x tcounts of
                    Just (i,c) -> (i,c+1)
                    Nothing    -> (Map.size tcounts,1)
  in (Map.insert x p tcounts,i)

addValue hist tag values x =
  Map.alter (Just . maybe (Set.singleton hist,Map.singleton tag 1)
                          (\(hists,tags) -> (Set.insert hist hists,Map.insertWith (+) tag 1 tags)))
            x values

keepPopulated !lindex val (hists,tags)
  | mustKeep  = (lindex+Map.size tags,(Right (Set.toList hists),annotate tags))
  | otherwise = (lindex              ,(Left  size              ,annotate tags))
  where
    mustKeep = case val of
                 CurrWord _ -> size > 2
                 _          -> size > 5
    size     = length hists

    annotate = zipWith (\i (t,c) -> (i,t,c)) [lindex..] . Map.toList

type Matrix = MV.IOVector Double

maxentProblem
  :: Double
  -> Map.Map h (Int, Int)
  -> Map.Map t (Int, Int)
  -> Map.Map f (Either Int [Int], [(Int, Int, Int)])
  -> (Function, Gradient, Combined)
maxentProblem sigmaSquared hcounts tcounts fvalues =
  (\lambda      -> do probConds <- MV.new (xSize*(ySize+1))
                      getLogLikelihood lambda probConds
  ,\lambda grad -> do probConds <- MV.new (xSize*(ySize+1))
                      getLogLikelihood lambda probConds
                      getGradient lambda probConds grad
  ,\lambda grad -> do probConds <- MV.new (xSize*(ySize+1))
                      l <- getLogLikelihood lambda probConds
                      getGradient lambda probConds grad
                      return l
  )
  where
    xSize = Map.size hcounts
    ySize = Map.size tcounts

    ht h t = h*(ySize+1)+t

    ptilde =
      VS.replicate xSize 0 VS.//
          [(i, fromIntegral c) | (i,c) <- Map.elems hcounts]

    getLogLikelihood :: PointMVector -> Matrix -> IO Double
    getLogLikelihood lambda probConds = do
      s <- populateProbConds probConds lambda (Map.elems fvalues) 0
      s <- updateZLambda probConds 0 s
      return s
      where
        populateProbConds probConds lambda []                           !s = return s
        populateProbConds probConds lambda ((Right hists,tags):fvalues) !s = do
          s <- updateProbConds probConds lambda tags s
          populateProbConds probConds lambda fvalues s
          where
            updateProbConds probConds lambda []             !s = return s
            updateProbConds probConds lambda ((i,t,c):tags) !s = do
              l <- MV.read lambda i
              mapM_  (\h -> do v <- MV.read probConds (ht h t)
                               MV.write probConds (ht h t) (v+l))
                     hists
              updateProbConds probConds lambda tags (s - fromIntegral c * l + (l*l)/(2*sigmaSquared))
        populateProbConds probConds lambda (_                 :fvalues) !s =
          populateProbConds probConds lambda fvalues s

        updateZLambda probConds h s
          | h < xSize = do zl <- logSum probConds h
                           MV.write probConds (ht h ySize) zl
                           divideZ probConds h 0 zl
                           updateZLambda probConds (h+1) (s + (ptilde VS.! h) * zl)
          | otherwise = do return s
          where
            logSum probConds h = do
              max <- getMax probConds 0 0
              sum <- getSum probConds 0 max 0
              return (max + log sum)

            getMax probConds t max
              | t < ySize = do v <- MV.read probConds (ht h t)
                               if v > max
                                 then getMax probConds (t+1) v
                                 else getMax probConds (t+1) max
              | otherwise = return max

            getSum probConds t max !sum
              | t < ySize = do v <- MV.read probConds (ht h t)
                               getSum probConds (t+1) max (sum+exp (v-max))
              | otherwise = return sum

            divideZ probConds h t zl
              | t < ySize = do v <- MV.read probConds (ht h t)
                               MV.write probConds (ht h t) (exp (v-zl))
                               divideZ probConds h (t+1) zl
              | otherwise = return ()

    getGradient :: PointMVector -> Matrix -> GradientMVector -> IO ()
    getGradient lambda probConds grad = do
      mapM_ (populateGrad lambda probConds grad) fvalues
      where
        populateGrad lambda probConds grad (Right hists,tags) =
          mapM_ (\(i,t,c) -> do s <- getSum probConds hists t (-fromIntegral c)
                                l <- MV.read lambda i
                                MV.write grad i (s + l/sigmaSquared))
                tags
        populateGrad lambda probConds grad (_          ,tags) =
          return ()

        getSum probConds []     t !s = return s
        getSum probConds (h:hs) t !s = do
          pc <- MV.read probConds (ht h t)
          getSum probConds hs t (s + (ptilde VS.! h) * pc)

readCorpus corpus_path =
  fmap (take 1000 . mapMaybe toExpr . lines) $ readFile corpus_path
  where
    toExpr ('a':'b':'s':':':' ':ls) = readExpr ls
    toExpr _                        = Nothing
  
readModel model_path = do
  (l:ls) <- fmap lines $ readFile model_path
  let ws    = Set.fromList (read l)
      probs = Map.fromList (map read ls)
  return (ws,probs)

writeModel model_path (words,probs) = do
  writeFile model_path (unlines (show (Set.toList words) : map show (Map.toList probs)))
