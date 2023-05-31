module Sasha.Parse where

import Data.Kind (Type)
import Data.Word (Word8)
import Data.Word8Set (Word8Set)
import Data.SOP

import qualified Data.ByteString as BS

import Sasha (sasha)
import Sasha.Internal.ERE (ERE)

type Grammar :: [Type] -> Type
data Grammar xs where
    OK    :: Grammar '[]
    (:>)  :: ERE -> Grammar xs -> Grammar xs
    (:.>) :: Word8Set -> Grammar xs -> Grammar (Word8 : xs)
    (:*>) :: ERE -> Grammar xs -> Grammar (BS.ByteString : xs)

infixr 4 :>, :*>, :.>

type Curry :: Type -> [Type] -> Type
type family Curry r xs where
    Curry r '[]    = r
    Curry r (x:xs) = x -> Curry r xs

runGrammar :: forall r xs. r -> (BS.ByteString -> Curry r xs) -> Grammar xs -> BS.ByteString -> r
runGrammar err kont g bs = runGrammar' g bs err $ \bs' -> uncurryAll (kont bs')

curryAll :: forall r xs. (NP I xs -> r) -> Curry r xs
curryAll = undefined

uncurryAll :: Curry r xs -> NP I xs -> r
uncurryAll f Nil = f
uncurryAll f (I x :* xs) = uncurryAll (f x) xs


runGrammar' :: Grammar xs -> BS.ByteString -> r -> (BS.ByteString -> NP I xs -> r) -> r
runGrammar' OK bs _err kont
    = kont bs Nil
runGrammar' (ere :*> g) bs  err kont
    -- | nullable ere = TODO

    | otherwise
    = sasha
      err
      [ (ere, \pfx sfx -> runGrammar' g sfx err $ \bs' xs -> kont bs' (I pfx :* xs))
      ]
      bs

runGrammar' (ere :> g) bs err kont
    -- | nullable ere = TODO

    | otherwise
    = sasha
      err
      [ (ere, \_pfx sfx -> runGrammar' g sfx err kont)
      ]
      bs
