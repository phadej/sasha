{-# LANGUAGE TemplateHaskell #-}
-- {-# OPTIONS_GHC -ddump-splices #-}
module Sasha.Example.SaTTH where

import Algebra.Lattice ((/\))

import qualified Data.ByteString  as BS

import Sasha.Example.Token
import Sasha.TTH

satthToken :: BS.ByteString -> Maybe (Tk, BS.ByteString, BS.ByteString)
satthToken = $$(satth
    -- Lexer specification of JSON(like) tokens.
    [ [|| TkSpace        ||] := plus (unions (map utf8Char (" \t\r\n")))
    , [|| TkBraceOpen    ||] := "{"
    , [|| TkBraceClose   ||] := "}"
    , [|| TkBracketOpen  ||] := "["
    , [|| TkBracketClose ||] := "]"
    , [|| TkComma        ||] := ","
    , [|| TkColon        ||] := ":"
    , [|| TkString       ||] := appends [ "\"", star (anyChar /\ complement (utf8Char '"')), "\"" ]
    , [|| TkNumber       ||] := plus digit
    , [|| TkTrue         ||] := "true"
    , [|| TkFalse        ||] := "false"
    , [|| TkNull         ||] := "null"
    ])


satthUtf8 :: BS.ByteString -> Maybe ((), BS.ByteString, BS.ByteString)
satthUtf8 = $$(satth [ [|| () ||] := anyUtf8Char ] )
