{-# LANGUAGE TemplateHaskellQuotes #-}
module Sasha.Internal.Word8Set (
    memberCode,
) where

import Data.WideWord.Word256 (Word256 (..))
import Data.Word             (Word64, Word8)
import Data.Word8Set         (Word8Set)

import qualified Data.Bits     as Bits
import qualified Data.Word8Set as W8S

import Language.Haskell.TH.Syntax


-- | Optimized routing to check membership when 'Word8Set' is statically known.
--
-- @
-- 'memberCode' c ws = [||'member' $$c $$(liftTyped ws) ||]
-- @
--
memberCode :: Code Q Word8 -> Word8Set -> Code Q Bool
memberCode c ws
    -- simple cases
    | W8S.null ws
    = [|| False ||]

    | W8S.isFull ws                   
    = [|| True ||]

    | W8S.size ws == 1
    = [|| $$c == $$(liftTyped (W8S.findMin ws)) ||]

    | W8S.size ws == 2
    = [|| $$c == $$(liftTyped (W8S.findMin ws)) || $$c == $$(liftTyped (W8S.findMax ws)) ||]

    -- continuos range
    | Just (l, r) <- W8S.isRange ws   
    = [|| $$(liftTyped l) <= $$c && $$c <= $$(liftTyped r) ||]

    -- low chars
    | Word256 0 0 0 x <- W8S.toWord256 ws
    = [|| $$c < 64 && Bits.testBit ($$(liftTyped x) :: Word64) (fromIntegral ($$c :: Word8)) ||]

    -- fallback
    | otherwise
    = [|| W8S.member $$c $$(liftTyped ws) ||]
