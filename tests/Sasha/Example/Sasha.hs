module Sasha.Example.Sasha (sashaToken, sashaUtf8) where

import Algebra.Lattice ((/\))

import qualified Data.ByteString  as BS

import Sasha.Example.Token
import Sasha

-- | Lexer specification of JSON(like) tokens.
grammar :: Sasha Tk
grammar =
    [ TkSpace        := plus (unions (map utf8Char (" \t\r\n")))
    , TkBraceOpen    := "{"
    , TkBraceClose   := "}"
    , TkBracketOpen  := "["
    , TkBracketClose := "]"
    , TkComma        := ","
    , TkColon        := ":"
    , TkString       := appends [ "\"", star (anyChar /\ complement (utf8Char '"')), "\"" ]
    , TkNumber       := plus digit
    , TkTrue         := "true"
    , TkFalse        := "false"
    , TkNull         := "null"
    ]

sashaToken :: BS.ByteString -> Maybe (Tk, BS.ByteString, BS.ByteString)
sashaToken = sasha grammar

sashaUtf8 :: BS.ByteString -> Maybe ((), BS.ByteString, BS.ByteString)
sashaUtf8 = sasha [ () := anyUtf8Char ]
