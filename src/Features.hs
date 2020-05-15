module Features(Extractor,Feature(..),
                left3words,naacl2003unknowns) where

import Context
import Data.Char

data Feature
  = None
  | LeftWord  Context
  | CurrWord  Context
  | RightWord Context
  | LeftTag   Context
  | Left2Tags Context Context
  | UCase
  | CNumber
  | Dash
  | AllCap
  | LetterDigitDash
  | AllCapitalized
  | UpperDigitDash
  | StartSentenceCap
  | MidSentenceCapC
  | MidSentenceCap
  | WordPref (Either Int String)
  | WordSuff (Either Int String)
  deriving (Eq,Ord,Show,Read)

type Extractor = [Context] -> Context -> [Context] -> Feature

left3words :: [Extractor]
left3words = [leftWord,currWord,rightWord,last2Tags,leftTag]

leftWord  (tag:left) example      right  = LeftWord  (wordOnly tag)
currWord       left  example      right  = CurrWord  (wordOnly example)
rightWord      left  example (tag:right) = RightWord (wordOnly tag)

leftTag   (w:left)   example right = LeftTag   (tagOnly w)
last2Tags (u:v:left) example right = Left2Tags (tagOnly u) (tagOnly v)

naacl2003unknowns :: [Extractor]
naacl2003unknowns = [
  ucase,cnumber,dash,allCap,letterDigitDash,allCapitalized,
  upperDigitDash,startSentenceCap,midSentenceCapC,midSentenceCap,
  wordSuff 1,wordSuff 2,wordSuff 3,wordSuff 4,
  wordPref 1,wordPref 2,wordPref 3,wordPref 4
  ]

ucase _ example _ =
  case wordOnly example of
    Word w | any isUpper w -> UCase
    _                      -> None

cnumber _ example _ =
  case wordOnly example of
    Word w | any isDigit w -> CNumber
    _                      -> None

dash _ example _ =
  case wordOnly example of
    Word w | elem '-' w -> Dash
           | otherwise  -> None

allCap _ example _ =
  case wordOnly example of
    Word w | all (not . isLower) w -> AllCap
    _                              -> None

letterDigitDash _ example _ =
  case wordOnly example of
    Word w |    any isLetter w 
             && any isDigit  w
             && any (=='-')  w -> LetterDigitDash
    _                          -> None

allCapitalized _ example _ =
  case wordOnly example of
    Word w | all isUpper w -> AllCapitalized
    _                      -> None

upperDigitDash _ example _ =
  case wordOnly example of
    Word w |    any isUpper w 
             && any isDigit w
             && any (=='-') w -> UpperDigitDash
    _                         -> None

startSentenceCap left example _ =
  case (left,wordOnly example) of
    (START:_, Word (c:w)) | isUpper c -> StartSentenceCap
    _                                 -> None

midSentenceCapC  left example _ =
  case (left,wordOnly example) of
    (t:_, Word (c:w)) | t /= START && isUpper c -> MidSentenceCapC
    _                                           -> None

midSentenceCap   left example _ =
  case (left,wordOnly example) of
    (t:_, Word w) | t /= START && any isUpper w -> MidSentenceCap
    _                                           -> None

wordPref n _ example _ =
  case wordOnly example of
    Word w | length w <= n -> WordPref (Left n)
           | otherwise     -> WordPref (Right (take n w))
    _                      -> None

wordSuff n _ example _ =
  case wordOnly example of
    Word w | length w <= n -> WordSuff (Left n)
           | otherwise     -> WordSuff (Right ((reverse . take n . reverse) w))
    _                      -> None
