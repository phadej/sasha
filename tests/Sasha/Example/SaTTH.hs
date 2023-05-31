{-# LANGUAGE TemplateHaskell #-}
-- {-# OPTIONS_GHC -ddump-splices #-}
module Sasha.Example.SaTTH (satthToken, satthUtf8) where

import Algebra.Lattice ((/\))

import qualified Data.ByteString as BS

import Sasha.Example.Token
import Sasha.TTH

mkToken :: tag -> BS.ByteString -> BS.ByteString -> Maybe (tag, BS.ByteString, BS.ByteString)
mkToken tk pfx sfx = Just (tk, pfx, sfx)

satthToken :: BS.ByteString -> Maybe (Tk, BS.ByteString, BS.ByteString)
satthToken = $$(satth
    [|| Nothing ||]
    -- Lexer specification of JSON(like) tokens.
    [ plus (unions (map utf8Char (" \t\r\n")))                            := \ pfx sfx -> [|| mkToken TkSpace        $$pfx $$sfx ||]
    , "{"                                                                 := \ pfx sfx -> [|| mkToken TkBraceOpen    $$pfx $$sfx ||]
    , "}"                                                                 := \ pfx sfx -> [|| mkToken TkBraceClose   $$pfx $$sfx ||]
    , "["                                                                 := \ pfx sfx -> [|| mkToken TkBracketOpen  $$pfx $$sfx ||]
    , "]"                                                                 := \ pfx sfx -> [|| mkToken TkBracketClose $$pfx $$sfx ||]
    , ","                                                                 := \ pfx sfx -> [|| mkToken TkComma        $$pfx $$sfx ||]
    , ":"                                                                 := \ pfx sfx -> [|| mkToken TkColon        $$pfx $$sfx ||]
    , appends [ "\"", star (anyChar /\ complement (utf8Char '"')), "\"" ] := \ pfx sfx -> [|| mkToken TkString       $$pfx $$sfx ||]
    , plus digit                                                          := \ pfx sfx -> [|| mkToken TkNumber       $$pfx $$sfx ||]
    , "true"                                                              := \ pfx sfx -> [|| mkToken TkTrue         $$pfx $$sfx ||]
    , "false"                                                             := \ pfx sfx -> [|| mkToken TkFalse        $$pfx $$sfx ||]
    , "null"                                                              := \ pfx sfx -> [|| mkToken TkNull         $$pfx $$sfx ||]
    ])


satthUtf8 :: BS.ByteString -> Maybe BS.ByteString
satthUtf8 = $$(satth [|| Nothing ||] [ anyUtf8Char := \_ sfx -> [|| Just $$sfx ||] ] )
