{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -ddump-splices #-}
module Sasha.Example.Parse where

import Sasha
import Sasha.TTH.Parse
import Sasha.Parse

import qualified Data.ByteString as BS

mkDay :: BS.ByteString -> BS.ByteString -> BS.ByteString -> BS.ByteString
    -> Maybe (BS.ByteString, BS.ByteString, BS.ByteString)
mkDay sfx y m d = if BS.null sfx then Just (y, m, d) else Nothing

-- Haskell
parseDay :: BS.ByteString -> Maybe (BS.ByteString, BS.ByteString, BS.ByteString)
parseDay = runGrammar Nothing mkDay $
    (digit <> digit <> digit <> digit) :*> "-" :>
    (digit <> digit)                   :*> "-" :>
    (digit <> digit)                   :*> OK
  where
    
-- Typed Template Haskell
parseDay' :: BS.ByteString -> Maybe (BS.ByteString, BS.ByteString, BS.ByteString)
parseDay' = $$(grammarCode [|| Nothing ||] [|| mkDay ||] $
    (digit <> digit <> digit <> digit) :*> "-" :>
    (digit <> digit)                   :*> "-" :>
    (digit <> digit)                   :*> OK)
