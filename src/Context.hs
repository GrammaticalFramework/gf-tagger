module Context where

import PGF2

type Token = String
data Context
  = START
  | Tag           Cat String
  | Word    Token
  | WordTag Token Cat String
  | BIND_TAG
  | END
  deriving (Show,Read,Eq,Ord)

wordOnly (Tag       _ _) = error "missing word"
wordOnly (Word    t)     = Word t
wordOnly (WordTag t _ _) = Word t
wordOnly tag             = tag

tagOnly (Word t)           = Word t
tagOnly (WordTag t cat an) = Tag cat an
tagOnly tag                = tag

showContext START              = "*START*"
showContext (Tag       cat an) = cat++" "++an
showContext (Word    w)        = show w
showContext (WordTag w cat an) = show w++"\t"++cat++" "++an
showContext BIND_TAG           = "*BIND*"
showContext END                = "*END*"
