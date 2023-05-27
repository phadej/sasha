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

import Data.Word (Word8)

import qualified Data.ByteString as BS

import Sasha.Internal.ERE

-- | Lexer grammar specification: regular expression and result builder function
-- which takes a prefix (the matching part) and a suffix (the rest of input).
type Sasha r = [(ERE, BS.ByteString -> BS.ByteString -> r)]

-- | Scan for a single token.
sasha
    :: forall r. r    -- ^ no match value
    -> Sasha r        -- ^ scanner rules definitions
    -> BS.ByteString  -- ^ input
    -> r              -- ^ result
sasha noMatch grammar input0 = go noMatch 0 input0 grammar
  where
    -- Note: acc has to be lazy
    go :: r -> Int -> BS.ByteString -> Sasha r -> r
    go acc !_ !_       [] = acc
    go acc !i !input   ts = case BS.uncons input of
        Nothing          -> acc
        Just (c, input') -> go (next accs acc) (i + 1) input' ts'
          where
            ts' = derivativeSasha c ts
            accs = [ case BS.splitAt (i + 1) input0 of (pfx, sfx) -> f pfx sfx | (ere, f) <- ts', nullable ere]

            next []    x = x
            next (x:_) _ = x

derivativeSasha :: Word8 -> Sasha r -> Sasha r
derivativeSasha c ts =
    [ (ere', f)
    | (ere,  f) <- ts
    , let ere' = derivative c ere
    , not (isEmpty ere')
    ]
