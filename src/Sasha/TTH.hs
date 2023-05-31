{-# LANGUAGE TemplateHaskellQuotes #-}
module Sasha.TTH (
    -- * SaTTH, staged Sasha the lexer.
    SaTTH,
    satth,
    satth',
    -- * ERE specification
    ERE,
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
    satisfy,
    digit,
) where

import Language.Haskell.TH (Code, CodeQ, Exp, Q)

import Control.Monad               (forM)
import Data.List                   (sortOn)
import Data.Map                    (Map)
import Data.Maybe                  (isJust, listToMaybe)
import Data.Ord                    (Down (..))
import Data.Word                   (Word8)
import Data.Word8Set               (Word8Set)
import Language.Haskell.TTH.LetRec (letrecE)

import qualified Data.ByteString     as BS
import qualified Data.Map.Strict     as Map
import qualified Data.Word8Set       as W8S
import qualified Language.Haskell.TH as TH

import Sasha.Internal.ERE
import Sasha.Internal.Word8Set (memberCode)

-- | Lexer grammar specification: regular expression and result builder function
-- which takes a prefix (the matching part) and a suffix (the rest of input).
type SaTTH r = [(ERE, Code Q BS.ByteString -> Code Q BS.ByteString -> Code Q r)]

-- | Scan for a single token.
satth
    :: forall r. Code Q r           -- ^ no match value
    -> SaTTH r                      -- ^ scanner rules definitions
    -> Code Q (BS.ByteString -> r)  -- ^ scanner code
satth noMatch rules = [|| \bs -> $$(satthImpl noMatch rules [|| bs ||]) bs ||]

satth'
    :: forall r. Code Q r           -- ^ no match value
    -> SaTTH r                      -- ^ scanner rules definitions
    -> Code Q BS.ByteString
    -> Code Q r
satth' noMatch rules bs = [|| $$(satthImpl noMatch rules bs) $$bs ||]

-- | Generate a scanner code.
satthImpl :: forall r. Code Q r -> SaTTH r -> Code Q BS.ByteString -> Code Q (BS.ByteString -> r)
satthImpl noMatch grammar0 input0 = letrecE
    (\_ -> "state")
    trans
    start
  where
    grammar0' :: SaTTH' r
    grammar0' =
        [ S i f ere
        | (i, (ere, f)) <- zip [0..] grammar0
        ]

    start :: Monad m => (SaTTH' r -> m (Code Q (R r))) -> m (Code Q (BS.ByteString -> r))
    start rec = do
        startCode <- rec grammar0'
        -- we assume that none of the tokens accepts an empty string,
        -- so we start without specifying last match.
        return [|| \input -> $$startCode $$noMatch (0 :: Int) input ||]

    trans :: Monad m => (SaTTH' r -> m (Code Q (R r))) -> SaTTH' r -> m (Code Q (R r))
    trans _rec grammar
        | emptySashaTTH grammar
        = return [|| \ !acc _ _ -> acc ||]

    trans  rec grammar = do
        -- if the input is not empty?
        let grammarM1 :: Map (SaTTH' r) Word8Set
            grammarM1 = Map.fromListWith W8S.union
                [ (derivativeSaTTH c grammar, W8S.singleton c)
                | c <- [ minBound .. maxBound ]
                ]

            -- non-empty map
            grammarM :: [(Word8Set, SaTTH' r, M r)]
            grammarM =
                [ (c, grammar', makeM input0 grammar')
                | (grammar', c) <- Map.toList grammarM1
                ]

        -- next states
        nexts0 <- forM grammarM $ \(ws, grammar', modify) -> do
            if emptySashaTTH grammar' then return (ws, NextEmpty, modify)
            else if epsSashaTTH grammar' then return (ws, NextEps, modify)
            else do
                next <- rec grammar'
                return (ws, Next next, modify)

        -- sort next states
        let nexts :: [(Word8Set, Next (Code Q (R r)), M r)]
            nexts = sortOn (\(ws, _, _) -> meas ws) nexts0

        -- transition case
        let caseAnalysis
                :: Code Q r
                -> Code Q Int
                -> Code Q Word8
                -> Code Q BS.ByteString
                -> Code Q r
            caseAnalysis acc pos c input' = caseTTH [|| () ||]
                [ (memberCode c ws, body)

                | (ws, mnext, modify) <- nexts
                , let body = case mnext of
                        NextEmpty -> acc
                        NextEps   -> modify acc [|| $$pos + 1 ||]
                        Next next -> [|| let !pos' = $$pos + 1 in $$next $$(modify acc [|| pos' ||]) pos' $$input' ||]
                ]

        let debugWarns :: Q ()
            debugWarns = return ()

        -- Note: acc should stay lazy
        return $ TH.bindCode_ debugWarns [|| \ acc !_pos !input -> case BS.uncons input of
            Nothing           -> acc
            Just (c, _input') -> $$(caseAnalysis [|| acc ||] [|| _pos ||] [|| c ||] [|| _input' ||])
            ||]

-------------------------------------------------------------------------------
-- Sorting transitions
-------------------------------------------------------------------------------

data Meas
    = MeasLite Word8Set
    | MeasCont !(Down Int) !Word8Set
    | MeasSize !Int !Word8Set
  deriving (Eq, Ord)

meas :: Word8Set -> Meas
meas ws
    | W8S.size ws < 2         = MeasLite ws
    | isJust (W8S.isRange ws) = MeasCont (Down (W8S.size ws)) ws
    | otherwise               = MeasSize (W8S.size ws) ws

-------------------------------------------------------------------------------
-- Aliases
-------------------------------------------------------------------------------

-- | Inner scanner function.
--
-- * previous match
-- * position
-- * input
--
type R r = r -> Int -> BS.ByteString -> r

-- | Last accept modifier.
type M r = Code Q r -> CodeQ Int -> CodeQ r

makeM :: forall r. Code Q BS.ByteString -> SaTTH' r -> M r
makeM input0 grammar acc pos = case acc' of
    Nothing -> acc
    Just f  -> [|| case BS.splitAt $$pos $$input0 of (_pfx, _sfx) -> $$(f [|| _pfx ||] [|| _sfx ||]) ||]
  where
    acc' :: Maybe (Code Q BS.ByteString -> Code Q BS.ByteString -> Code Q r)
    acc' = listToMaybe
        [ f
        | S _ f ere <- grammar
        , nullable ere
        ]

data Next a
    = NextEmpty
    | NextEps
    | Next a

-------------------------------------------------------------------------------
-- TTH extras
-------------------------------------------------------------------------------

caseTTH :: Code Q a -> [(Code Q Bool, CodeQ r)] -> Code Q r
caseTTH c guards = TH.unsafeCodeCoerce $ TH.caseE (TH.unTypeCode c)
    [ TH.match TH.wildP (TH.guardedB (go guards))  [] ]
  where
    go :: [(Code Q Bool, Code Q r)] -> [Q (TH.Guard, Exp)]
    go []          = []
    go [(_,b)]     = [TH.normalGE [| otherwise |] (TH.unTypeCode b)]
    go ((g,b):gbs) = TH.normalGE (TH.unTypeCode g) (TH.unTypeCode b) : go gbs

-------------------------------------------------------------------------------
-- State
-------------------------------------------------------------------------------

-- | We give each tag an integer, so we can order them.
data S r = S !Int !(Code Q BS.ByteString -> Code Q BS.ByteString -> Code Q r) !ERE

instance Show (S tag) where
    show (S i _ ere) = show (i, ere)

instance Eq (S tag) where
    S i _ ere == S i' _ ere' = (i, ere) == (i', ere')

instance Ord (S tag) where
    compare (S i _ ere) (S i' _ ere') = compare (i, ere) (i', ere')

type SaTTH' tag = [S tag]

-------------------------------------------------------------------------------
-- Derivative
-------------------------------------------------------------------------------

derivativeSaTTH :: Word8 -> SaTTH' tag -> SaTTH' tag
derivativeSaTTH c ts =
    [ S i code ere''
    | S i code ere <- ts
    , let ere' = derivative c ere
    , let ere'' = simplifyERE ere'
    , not (equivalent empty ere'')
    ]

simplifyERE :: ERE -> ERE
simplifyERE ere
    | equivalent ere eps = eps
    | otherwise          = ere

-- does it make sense to look further?
emptySashaTTH :: SaTTH' tag -> Bool
emptySashaTTH = null

epsSashaTTH :: SaTTH' tag -> Bool
epsSashaTTH grammar = and [ equivalent ere eps | S _ _ ere <- grammar ]
