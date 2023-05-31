{-# LANGUAGE TemplateHaskellQuotes #-}
module Sasha.TTH.Parse where

import Data.SOP.NP
import Language.Haskell.TH (Code, CodeQ, Exp, Q)
import Data.Kind (Type)
import Data.Word (Word8)
import Data.Word8Set (Word8Set)

import qualified Data.ByteString as BS


import Sasha.TTH
import Sasha.Parse

grammarCode
    :: forall r xs. Code Q r -- ^ on non-match
    -> Code Q (BS.ByteString -> Curry r xs) -- ^ on match
    -> Grammar xs
    -> Code Q (BS.ByteString -> r)
grammarCode err kont g = 
    [|| \bs -> $$(grammarCode'
        g
        [|| bs ||]
        err (\sfx nps -> codeUncurryNP (codeApp kont sfx) nps)) ||]

codeApp :: Code Q (a -> b) -> Code Q a -> Code Q b
codeApp f x = [|| $$f $$x ||]

codeUncurryNP :: Code Q (Curry r xs) -> NP (Code Q) xs -> Code Q r
codeUncurryNP f Nil = f
codeUncurryNP f (x :* xs) = codeUncurryNP (codeApp f x) xs

grammarCode'
    :: Grammar xs
    -> Code Q BS.ByteString
    -> Code Q r
    -> (Code Q BS.ByteString -> NP (Code Q) xs -> Code Q r)
    -> Code Q r
grammarCode' OK bs _err kont
    = kont bs Nil
grammarCode' (ere :*> g) bs err kont
    -- | nullable ere = TODO

    | otherwise
    = satth' err
        [ (ere, \pfx sfx -> grammarCode' g  sfx err $ \bs' xs -> kont bs' (pfx :* xs))
        ]
        bs

grammarCode' (ere :> g) bs err kont
    -- | nullable ere = TODO

    | otherwise
    = satth' err
        [ (ere, \_pfx sfx -> grammarCode' g sfx err kont)
        ]
        bs
