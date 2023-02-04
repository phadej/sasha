module Sasha (
    -- * Sasha the lexer

    -- | This is the ordinary Haskell (i.e. slow) interface.
    --
    -- The fast one is in "Sasha.TTH" module, but that requires @TemplateHaskell@.
    --
    Sasha,
    sasha,
    -- * ERE specification
    ERE,
    empty,
    eps,
    char,
    charRange,
    utf8Char,
    anyChar,
    anyUtf8Char,
    appends,
    unions,
    intersections,
    star,
    plus,
    string,
    utf8String,
    complement,
    satisfy,
    digit,
) where

import Control.Applicative ((<|>))
import Data.Maybe          (listToMaybe)
import Data.Word           (Word8)

import qualified Data.ByteString as BS

import Sasha.Internal.ERE

-- | Lexer grammar specification: tags and regular expressions.
type Sasha tag = [(tag, ERE)]

-- | Scan for a single token.
sasha
    :: forall tag. Sasha tag                      -- ^ scanner definition
    -> BS.ByteString                              -- ^ input
    -> Maybe (tag, BS.ByteString, BS.ByteString)  -- ^ matched token, consumed bytestring, left over bytestring
sasha grammar input0 = finish <$> go Nothing 0 input0 grammar
  where
    finish :: (tag, Int) -> (tag, BS.ByteString, BS.ByteString)
    finish (tag, i) = case BS.splitAt i input0 of
        (pfx, sfx) -> (tag, pfx, sfx)

    go :: Maybe (tag, Int) -> Int -> BS.ByteString -> Sasha tag -> Maybe (tag, Int)
    go acc !_   _       [] = acc
    go acc !pfx input   ts = case BS.uncons input of
        Nothing -> acc' <|> acc
          where
            acc' :: Maybe (tag, Int)
            acc' = listToMaybe [ (tag, pfx) | (tag, ere) <- ts, nullable ere ]
        Just (c, sfx) -> go (acc' <|> acc) (pfx + 1) sfx ts'
          where
            ts' = derivativeSasha c ts
            acc' = listToMaybe [ (tag, pfx + 1) | (tag, ere) <- ts', nullable ere]

derivativeSasha :: Word8 -> Sasha tag -> Sasha tag
derivativeSasha c ts =
    [ (t, ere')
    | (t, ere) <- ts
    , let ere' = derivative c ere
    , not (isEmpty ere')
    ]
