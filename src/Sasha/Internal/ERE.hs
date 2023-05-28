{-# LANGUAGE BangPatterns           #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE PatternGuards          #-}
{-# LANGUAGE ScopedTypeVariables    #-}
module Sasha.Internal.ERE (
    ERE (..),
    -- * Construction
    --
    -- | Binary operators are
    --
    -- * '<>' for append
    -- * '\/' for union
    -- * '/\' for intersection
    --
    empty,
    eps,
    char,
    charRange,
    charSet,
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
    everything,
    satisfy,
    digit,
    -- * Equivalence
    equivalent,
    -- * Derivative
    nullable,
    derivative,
    match,
    -- * Other
    isEmpty,
    isEverything,
    ) where

import Algebra.Lattice
       (BoundedJoinSemiLattice (..), BoundedMeetSemiLattice (..), Lattice (..))
import Data.Bits       (shiftR, (.&.), (.|.))
import Data.Char       (ord)
import Data.Foldable   (toList)
import Data.Set        (Set)
import Data.String     (IsString (..))
import Data.Word       (Word8)
import Data.Word8Set   (Word8Set)
import Test.QuickCheck (Arbitrary (..))

import qualified Data.Set        as Set
import qualified Data.Word8Set   as W8S
import qualified Test.QuickCheck as QC

-------------------------------------------------------------------------------
-- Doctest
-------------------------------------------------------------------------------

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Control.Monad (void)
-- >>> import Data.Foldable (traverse_)
-- >>> import Data.List (sort)
-- >>> import Algebra.Lattice ((/\), (\/))
--
-- >>> import Test.QuickCheck ((===))
-- >>> import qualified Test.QuickCheck as QC
--
-------------------------------------------------------------------------------
-- ERE
-------------------------------------------------------------------------------

-- | Extended regular expression
--
data ERE
    = EREAppend [ERE]              -- ^ Concatenation
    | EREUnion Word8Set (Set ERE)  -- ^ Union
    | EREStar ERE                  -- ^ Kleene star
    | ERENot ERE                   -- ^ Complement
  deriving (Eq, Ord, Show)

-------------------------------------------------------------------------------
-- Smart constructor
-------------------------------------------------------------------------------

-- | Empty regex. Doesn't accept anything.
--
-- prop> match empty s === False
--
empty :: ERE
empty = EREUnion W8S.empty Set.empty

-- | Everything.
--
-- prop> match everything s === True
--
everything :: ERE
everything = complement empty

-- | Empty string. /Note:/ different than 'empty'.
--
-- prop> match eps s === null s
--
eps :: ERE
eps = EREAppend []

-- | Character.
--
char :: Word8 -> ERE
char c = EREUnion (W8S.singleton c) Set.empty

-- | Character range.
--
charRange :: Word8 -> Word8 -> ERE
charRange l u = charSet (W8S.range l u)

-- | Character set.
--
-- @since 0.2
charSet :: Word8Set -> ERE
charSet s = EREUnion s Set.empty

-- | Any character.
--
anyChar :: ERE
anyChar = EREUnion W8S.full Set.empty

anyUtf8Char :: ERE
anyUtf8Char = unions
    [ charRange 0x00 0x7F
    , charRange 0xC2 0xDF <> charRange 0x80 0xBF
    , charRange 0xE0 0xE0 <> charRange 0xa0 0xBF <> charRange 0x80 0xBF
    , charRange 0xE1 0xEC <> charRange 0x80 0xBF <> charRange 0x80 0xBF
    , charRange 0xED 0xED <> charRange 0x80 0x9F <> charRange 0x80 0xBF
    , charRange 0xEE 0xEF <> charRange 0x80 0xBF <> charRange 0x80 0xBF
    , charRange 0xF0 0xF0 <> charRange 0x90 0xBF <> charRange 0x80 0xBF <> charRange 0x80 0xBF
    , charRange 0xF1 0xF3 <> charRange 0x80 0xBF <> charRange 0x80 0xBF <> charRange 0x80 0xBF
    , charRange 0xF4 0xF4 <> charRange 0x80 0x8f <> charRange 0x80 0xBF <> charRange 0x80 0xBF
    ]

-- | Concatenate regular expressions.
--
-- prop> r <> empty === empty
-- prop> empty <>  r === empty
-- prop> ( r <> s) <> t === r <> (s <> t)
--
-- prop>  r <> eps === r
-- prop> eps <>  r === r
--
appends :: [ERE] -> ERE
appends rs0
    | elem empty rs1 = empty
    | otherwise = case rs1 of
        [r] -> r
        rs  -> EREAppend rs
  where
    -- flatten one level of EREAppend
    rs1 = concatMap f rs0

    f (EREAppend rs) = rs
    f r             = [r]

-- | Union of regular expressions.
--
-- prop>  r \/ r === r
-- prop>  r \/ s === s \/ r
-- prop> ( r \/ s) \/ t === r \/ (s \/ t)
--
-- prop> empty \/  r === r
-- prop>  r \/ empty === r
--
-- prop> everything \/  r === everything
-- prop>  r \/ everything === everything
--
unions :: [ERE] -> ERE
unions = uncurry mk . foldMap f where
    mk cs rss
        | Set.null rss = EREUnion cs Set.empty
        | Set.member everything rss = everything
        | W8S.null cs = case Set.toList rss of
            []  -> empty
            [r] -> r
            _   -> EREUnion cs rss
        | otherwise    = EREUnion cs rss

    f (EREUnion cs rs) = (cs, rs)
    f r                = (W8S.empty, Set.singleton r)

-- | Intersection of regular expressions.
--
-- prop>  r /\ r === r
-- prop>  r /\ s === s /\ r
-- prop> ( r /\ s) /\ t === r /\ (s /\ t)
--
-- prop> empty /\  r === empty
-- prop>  r /\ empty === empty
--
-- prop> everything /\  r === r
-- prop>  r /\ everything === r
--
intersections :: [ERE] -> ERE
intersections = complement . unions . map complement

-- | Complement.
--
-- prop> complement (complement r) ===  r
--
complement :: ERE -> ERE
complement r = case r of
    ERENot r'                    -> r'
    _                            -> ERENot r

-- | Kleene star.
--
-- prop> star (star r) === star ( r)
--
-- prop> star eps     ===  eps
-- prop> star empty   ===  eps
-- prop> star anyChar ===  everything
--
-- prop> star (r \/ eps) === star r
-- prop> star (char c \/ eps) === star (char c)
-- prop> star (empty \/ eps) === eps
--
star :: ERE -> ERE
star r = case r of
    EREStar _                          -> r
    EREAppend []                       -> eps
    EREUnion cs rs
        | W8S.null cs, Set.null rs     -> eps
        | W8S.isFull cs, Set.null rs   -> everything
        | Set.member eps rs -> case Set.toList rs' of
            []                  -> star (EREUnion cs Set.empty)
            [r'] | W8S.null cs  -> star r'
            _                   -> EREStar (EREUnion cs rs')
          where
            rs' = Set.delete eps rs
    _                                  -> EREStar r

-- | Kleene plus
--
-- @
-- 'plus' r = r <> 'star' r
-- @
plus :: ERE -> ERE
plus r = r <> star r

-- | Literal string.
--
string :: [Word8] -> ERE
string []  = eps
string [c] = EREUnion (W8S.singleton c) Set.empty
string cs  = EREAppend $ map char cs

-- | UTF8 string
utf8String :: String -> ERE
utf8String = string . concatMap encodeCharUtf8

-- | UTF8 character, i.e. may match multiple bytes.
utf8Char :: Char -> ERE
utf8Char = string . encodeCharUtf8

satisfy :: (Word8 -> Bool) -> ERE
satisfy p = EREUnion (W8S.fromList [ x | x <- [ minBound .. maxBound], p x ]) Set.empty

digit :: ERE
digit = charRange (fromIntegral (ord '0')) (fromIntegral (ord '9'))

-------------------------------------------------------------------------------
-- derivative
-------------------------------------------------------------------------------

instance Semigroup ERE where
    r <> r' = appends [r, r']

instance Monoid ERE where
    mempty  = eps
    mappend = (<>)
    mconcat = appends

instance Lattice ERE where
    r \/ r' = unions [r, r']
    r /\ r' = intersections [r, r']

instance BoundedJoinSemiLattice ERE where
    bottom = empty

instance BoundedMeetSemiLattice ERE where
    top = everything

-- | Uses 'utf8string'.
instance IsString ERE where
    fromString = utf8String

-- | Uses smart constructors.
instance Arbitrary ERE where
    arbitrary = QC.sized arb
      where
        arb n | n <= 1    = QC.frequency
            [ (20, EREUnion <$> arbitrary <*> pure Set.empty)
            , (1, pure eps)
            , (1, pure empty)
            ]
              | otherwise = QC.oneof
            [ do
                p <- arbPartition (n - 1)
                unions <$> traverse arb p
            , do
                p <- arbPartition (n - 1)
                appends <$> traverse arb p

            , star <$> arb (n - 1)
            , complement <$> arb (n - 1)
            ]

    shrink (EREAppend rs) = rs
    shrink (EREStar r)    = r : map EREStar (shrink r)
    shrink (ERENot r)     = r : map ERENot (shrink r)
    shrink _              = []

arbPartition :: Int -> QC.Gen [Int]
arbPartition k = case compare k 1 of
    LT -> pure []
    EQ -> pure [1]
    GT -> do
        first <- QC.chooseInt (1, k)
        rest <- arbPartition $ k - first
        QC.shuffle (first : rest)



-------------------------------------------------------------------------------
-- derivative
-------------------------------------------------------------------------------

-- | We say that a regular expression r is nullable if the language it defines
-- contains the empty string.
--
-- >>> nullable eps
-- True
--
-- >>> nullable (star "x")
-- True
--
-- >>> nullable "foo"
-- False
--
-- >>> nullable (complement eps)
-- False
--
nullable :: ERE -> Bool
nullable (EREAppend rs)    = all nullable rs
nullable (EREUnion _cs rs) = any nullable rs
nullable (EREStar _)       = True
nullable (ERENot r)        = not (nullable r)

-- | Intuitively, the derivative of a language \(\mathcal{L} \subset \Sigma^\star\)
-- with respect to a symbol \(a \in \Sigma\) is the language that includes only
-- those suffixes of strings with a leading symbol \(a\) in \(\mathcal{L}\).
--
derivative :: Word8 -> ERE -> ERE
derivative c (EREUnion cs rs)  = unions $ derivativeChars c cs : [ derivative c r | r <- toList rs]
derivative c (EREAppend rs)    = derivativeAppend c rs
derivative c rs@(EREStar r)    = derivative c r <> rs
derivative c (ERENot r)        = complement (derivative c r)

derivativeAppend :: Word8 -> [ERE] -> ERE
derivativeAppend _ []      = empty
derivativeAppend c [r]     = derivative c r
derivativeAppend c (r:rs)
    | nullable r         = unions [r' <> appends rs, rs']
    | otherwise          = r' <> appends rs
  where
    r'  = derivative c r
    rs' = derivativeAppend c rs

derivativeChars :: Word8 -> Word8Set -> ERE
derivativeChars c cs
    | c `W8S.member` cs      = eps
    | otherwise              = empty

match :: ERE -> [Word8] -> Bool
match ere []     = nullable ere
match ere (w:ws) = match (derivative w ere) ws

-------------------------------------------------------------------------------
-- isEmpty
-------------------------------------------------------------------------------

-- | Whether 'ERE' is (structurally) equal to 'empty'.
isEmpty :: ERE -> Bool
isEmpty (EREUnion cs rs) = W8S.null cs && Set.null rs
isEmpty _                = False

-- | Whether 'ERE' is (structurally) equal to 'everything'.
isEverything :: ERE -> Bool
isEverything (ERENot (EREUnion cs rs)) = W8S.null cs && Set.null rs
isEverything _                         = False

-------------------------------------------------------------------------------
-- Utf8
-------------------------------------------------------------------------------

encodeCharUtf8 :: Char -> [Word8]
encodeCharUtf8 c
  | c <= '\x07F'  = w8
                  : []
  | c <= '\x7FF'  = (0xC0 .|.  w8ShiftR  6          )
                  : (0x80 .|. (w8          .&. 0x3F))
                  : []
  | c <= '\xD7FF' = (0xE0 .|.  w8ShiftR 12          )
                  : (0x80 .|. (w8ShiftR  6 .&. 0x3F))
                  : (0x80 .|. (w8          .&. 0x3F))
                  : []
  | c <= '\xDFFF' = 0xEF : 0xBF : 0xBD -- U+FFFD
                  : []
  | c <= '\xFFFF' = (0xE0 .|.  w8ShiftR 12          )
                  : (0x80 .|. (w8ShiftR  6 .&. 0x3F))
                  : (0x80 .|. (w8          .&. 0x3F))
                  : []
  | otherwise     = (0xf0 .|.  w8ShiftR 18          )
                  : (0x80 .|. (w8ShiftR 12 .&. 0x3F))
                  : (0x80 .|. (w8ShiftR  6 .&. 0x3F))
                  : (0x80 .|. (w8          .&. 0x3F))
                  : []
  where
    w8 :: Word8
    w8 = fromIntegral (ord c)

    w8ShiftR :: Int -> Word8
    w8ShiftR b = fromIntegral (shiftR (ord c) b)

-------------------------------------------------------------------------------
-- Equivalance
-------------------------------------------------------------------------------

equivalent :: ERE -> ERE -> Bool
equivalent x0 y0 = agree (x0, y0) && go mempty [(x0, y0)] []
  where
    -- we use two queues, so we can append chunks cheaply.
    go :: Set (ERE, ERE) -> [(ERE, ERE)] -> [[(ERE,ERE)]] -> Bool
    go !_  []              []       = True
    go acc []              (zs:zss) = go acc zs zss
    go acc (p@(x, y) : zs) zss
        | p `Set.member` acc = go acc zs zss
        -- if two regexps are structurally the same, we don't need to recurse.
        | x == y             = go (Set.insert p acc) zs zss
        | all agree ps       = go (Set.insert p acc) zs (ps : zss)
        | otherwise          = False
      where
        cs = [minBound .. maxBound] :: [Word8]
        ps = map (\c -> (derivative c x, derivative c y)) cs

    agree :: (ERE, ERE) -> Bool
    agree (x, y) = nullable x == nullable y
