{-# LANGUAGE TemplateHaskellQuotes #-}
module Sasha.Internal.Word8Set (
    -- * Set type
    Word8Set,
    Key,

    -- * Construction
    empty,
    full,
    singleton,
    range,
    fromList,

    -- * Insertion
    insert,

    -- * Deletion
    delete,

    -- * Query
    member,
    memberCode,
    isSubsetOf,
    null,
    isFull,
    isSingleRange,
    size,

    -- * Combine
    union,
    intersection,
    complement,

    -- * Min\/Max
    findMin,
    findMax,
    -- * Conversion to List
    elems,
    toList,
) where

import Prelude
       (Bool (..), Eq ((==)), Int, Monoid (..), Ord, Semigroup (..),
       Show (showsPrec), fromIntegral, negate, otherwise, showParen, showString,
       ($), (&&), (+), (-), (.), (<), (<=), (>), (||), return)

import Data.Bits             ((.&.), (.|.))
import Data.Foldable         (foldl')
import Data.WideWord.Word256 (Word256 (..))
import Data.Word             (Word64, Word8)
import Test.QuickCheck       (Arbitrary (..))
import Algebra.Lattice
       (BoundedJoinSemiLattice (..), BoundedMeetSemiLattice (..), Lattice (..))

import Language.Haskell.TH.Syntax

import qualified Data.Bits as Bits

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

newtype Word8Set = W8S Word256
  deriving (Eq, Ord)

type Key = Word8

-------------------------------------------------------------------------------
-- Instances
-------------------------------------------------------------------------------

instance Show Word8Set where
    showsPrec d xs = showParen (d > 10) $ showString "fromList " . showsPrec 11 (toList xs)

instance Lift Word8Set where
    liftTyped (W8S (Word256 a b c d)) =
        [|| W8S (Word256 a b c d) ||]

instance Semigroup Word8Set where
    (<>) = union

instance Monoid Word8Set where
    mempty = empty

instance Arbitrary Word8Set where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        c <- arbitrary
        d <- arbitrary
        return (W8S (Word256 a b c d))

instance Lattice Word8Set where
    (\/) = union
    (/\) = intersection

instance BoundedJoinSemiLattice Word8Set where
    bottom = empty

instance BoundedMeetSemiLattice Word8Set where
    top = full

-------------------------------------------------------------------------------
-- Construction
-------------------------------------------------------------------------------

empty :: Word8Set
empty = W8S Bits.zeroBits

full :: Word8Set
full = W8S ones

ones :: Word256
ones = Bits.complement Bits.zeroBits

singleton :: Word8 -> Word8Set
singleton x = W8S (Bits.bit (fromIntegral x))

range :: Word8 -> Word8 -> Word8Set
range mi ma
    | mi <= ma  = W8S $ Bits.shiftL (Bits.shiftR ones (fromIntegral (negate (1 + ma - mi)))) (fromIntegral mi)
    | otherwise = empty

-------------------------------------------------------------------------------
-- Insertion
-------------------------------------------------------------------------------

insert :: Word8 -> Word8Set -> Word8Set
insert x (W8S xs) = W8S (Bits.setBit xs (fromIntegral x))

-------------------------------------------------------------------------------
-- Deletion
-------------------------------------------------------------------------------

delete :: Word8 -> Word8Set -> Word8Set
delete x (W8S xs) = W8S (Bits.clearBit xs (fromIntegral x))

-------------------------------------------------------------------------------
-- Query
-------------------------------------------------------------------------------

null :: Word8Set -> Bool
null (W8S xs) = xs == Bits.zeroBits

isFull :: Word8Set -> Bool
isFull (W8S xs) = xs == ones

size :: Word8Set -> Int
size (W8S xs) = Bits.popCount xs

member :: Word8 -> Word8Set -> Bool
member x (W8S xs) = Bits.testBit xs (fromIntegral x)

-- | Optimized routing to check membership when 'Word8Set' is statically known.
--
-- @
-- 'memberCode' c ws = [||'member' $$c $$(liftTyped ws) ||]
-- @
--
memberCode :: Code Q Word8 -> Word8Set -> Code Q Bool
memberCode c ws
    -- simple cases
    | null ws                     = [|| False ||]
    | isFull ws                   = [|| True ||]
    | size ws == 1                = [|| $$c == $$(liftTyped (findMin ws)) ||]
    | size ws == 2                = [|| $$c == $$(liftTyped (findMin ws)) || $$c == $$(liftTyped (findMax ws)) ||]

    -- continuos range
    | isSingleRange ws            = [|| $$(liftTyped (findMin ws)) <= $$c && $$c <= $$(liftTyped (findMax ws)) ||]

    -- low chars
    | W8S (Word256 0 0 0 x) <- ws = [|| $$c < 64 && Bits.testBit ($$(liftTyped x) :: Word64) (fromIntegral ($$c :: Word8)) ||]

    -- fallback
    | otherwise                   = [|| member $$c $$(liftTyped ws) ||]

isSubsetOf :: Word8Set -> Word8Set -> Bool
isSubsetOf a b = b == union a b

isSingleRange :: Word8Set -> Bool
isSingleRange (W8S 0)  = True
isSingleRange (W8S ws) = Bits.popCount ws + Bits.countLeadingZeros ws + Bits.countTrailingZeros ws == 256

-------------------------------------------------------------------------------
-- Combine
-------------------------------------------------------------------------------

complement :: Word8Set -> Word8Set
complement (W8S xs) = W8S (Bits.complement xs)

union :: Word8Set -> Word8Set -> Word8Set
union (W8S xs) (W8S ys) = W8S (xs .|. ys)

intersection :: Word8Set -> Word8Set -> Word8Set
intersection (W8S xs) (W8S ys) = W8S (xs .&. ys)

-------------------------------------------------------------------------------
-- Min/Max
-------------------------------------------------------------------------------

findMin :: Word8Set -> Word8
findMin (W8S xs) = fromIntegral (Bits.countTrailingZeros xs)

findMax :: Word8Set -> Word8
findMax (W8S xs) = fromIntegral (255 - Bits.countLeadingZeros xs)

-------------------------------------------------------------------------------
-- List
-------------------------------------------------------------------------------

elems :: Word8Set -> [Word8]
elems = toList

toList :: Word8Set -> [Word8]
toList xs = [ w8 | w8 <- [0x00..0xff], member w8 xs]

fromList :: [Word8] -> Word8Set
fromList w8s = W8S $ foldl' (\acc w8 -> Bits.setBit acc (fromIntegral w8)) Bits.zeroBits w8s
