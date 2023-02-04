module Sasha.Example.Token where

import Control.DeepSeq (NFData (rnf))

data Tk
    = TkErr
    | TkSpace
    | TkBraceOpen
    | TkBraceClose
    | TkBracketOpen
    | TkBracketClose
    | TkComma
    | TkColon
    | TkString
    | TkNumber
    | TkTrue
    | TkFalse
    | TkNull
  deriving (Eq, Show)

instance NFData Tk where
    rnf !_ = ()

pattern (:=) :: a -> b -> (a, b)
pattern x := y = (x, y)
