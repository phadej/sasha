module Sasha.Example.Sasha (sashaToken, sashaUtf8) where

import Algebra.Lattice ((/\))

import qualified Data.ByteString as BS

import Sasha.Example.Token
import Sasha

mkToken :: tag -> BS.ByteString -> BS.ByteString -> Maybe (tag, BS.ByteString, BS.ByteString)
mkToken tk pfx sfx = Just (tk, pfx, sfx)

-- | Lexer specification of JSON(like) tokens.
grammar :: Sasha (Maybe (Tk, BS.ByteString, BS.ByteString))
grammar =
    [ plus (unions (map utf8Char (" \t\r\n")))                            := mkToken TkSpace
    , "{"                                                                 := mkToken TkBraceOpen
    , "}"                                                                 := mkToken TkBraceClose
    , "["                                                                 := mkToken TkBracketOpen
    , "]"                                                                 := mkToken TkBracketClose
    , ","                                                                 := mkToken TkComma
    , ":"                                                                 := mkToken TkColon
    , appends [ "\"", star (anyChar /\ complement (utf8Char '"')), "\"" ] := mkToken TkString
    , plus digit                                                          := mkToken TkNumber
    , "true"                                                              := mkToken TkTrue
    , "false"                                                             := mkToken TkFalse
    , "null"                                                              := mkToken TkNull
    ]

sashaToken :: BS.ByteString -> Maybe (Tk, BS.ByteString, BS.ByteString)
sashaToken = sasha Nothing grammar

sashaUtf8 :: BS.ByteString -> Maybe BS.ByteString
sashaUtf8 = sasha Nothing [ anyUtf8Char := \_pfx sfx -> Just sfx ]
