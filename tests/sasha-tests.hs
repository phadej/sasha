{-# LANGUAGE CPP #-}
module Main (main) where

import Data.Either           (isRight)
import System.Environment    (getArgs, withArgs)
import Test.Tasty            (defaultMain, testGroup)
import Test.Tasty.HUnit      (testCase, (@?=))
import Test.Tasty.QuickCheck (label, testProperty, (===))

import qualified Data.Aeson         as A
import qualified Data.ByteString    as BS
import qualified Data.Text.Encoding as TE
import qualified Test.Tasty.Bench   as B

import Sasha.Example.Alex
import Sasha.Example.Sasha
import Sasha.Example.SaTTH
import Sasha.Example.Token

main :: IO ()
main = do
    input <- BS.readFile "example.json"
    args  <- getArgs
    case args of
        "bench" : args' -> withArgs args' $ B.defaultMain
            [ B.bgroup "json"
                [ B.bench "sasha" $ B.nf (tokens sashaToken)       input
                , B.bench "alex"  $ B.nf (tokens alexToken)        input
                , B.bench "satth" $ B.nf (tokens satthToken)       input
                , B.bench "aeson" $ B.nf (A.decodeStrict @A.Value) input
                ]
            , B.bgroup "utf8"
                [ B.bench "sasha"      $ B.whnf (accepts sashaUtf8) input
                , B.bench "satth"      $ B.whnf (accepts satthUtf8) input
#if MIN_VERSION_bytestring(0,11,2)
                , B.bench "bytestring" $ B.whnf BS.isValidUtf8 input
#endif
                ]
            ]

        _ -> defaultMain $ testGroup "sasha-json"
            [ testGroup "json"
                [ testCase "sasha" $ tokens sashaToken input @?= expectedJson
                , testCase "satth" $ tokens satthToken input @?= expectedJson
                , testCase "alex"  $ tokens alexToken  input @?= expectedJson
                ]
            , testGroup "utf8"
                [ testProperty "sasha" $ \w8s ->
                    let bs = BS.pack w8s
                        isValid = isRight (TE.decodeUtf8' bs)
                    in label (show isValid) $ accepts sashaUtf8 bs === isValid

                , testProperty "sasha" $ \w8s ->
                    let bs = BS.pack w8s
                        isValid = isRight (TE.decodeUtf8' bs)
                    in label (show isValid) $ accepts satthUtf8 bs === isValid
                ]
            ]

tokens
    :: (BS.ByteString -> Maybe (Tk, BS.ByteString, BS.ByteString))  -- ^ single token scanner
    -> BS.ByteString                                                -- ^ input
    -> [(Tk, BS.ByteString)]                                        -- ^ result list of tokens
tokens scan = go
  where
    go !bs
        | BS.null bs = []
        | otherwise  = case scan bs of
            Nothing             -> [(TkErr, bs)]
            Just (tk, pfx, sfx) -> (tk, pfx) : go sfx
{-# INLINE tokens #-}

accepts
    :: (BS.ByteString -> Maybe BS.ByteString)  -- ^ single token scanner
    -> BS.ByteString                           -- ^ input
    -> Bool
accepts scan = go
  where
    go !bs
        | BS.null bs = True
        | otherwise  = case scan bs of
            Nothing  -> False
            Just sfx -> go sfx
{-# INLINE accepts #-}

expectedJson :: [(Tk, BS.ByteString)]
expectedJson =
    [ TkBraceOpen    := "{"
    , TkSpace        := "\n    "
    , TkString       := "\"literals\""
    , TkColon        := ":"
    , TkSpace        := " "
    , TkBracketOpen  := "["
    , TkTrue         := "true"
    , TkComma        := ","
    , TkSpace        := " "
    , TkFalse        := "false"
    , TkComma        := ","
    , TkSpace        := " "
    , TkNull         := "null"
    , TkBracketClose := "]"
    , TkComma        := ","
    , TkSpace        := "\n    "
    , TkString       := "\"numbers\""
    , TkColon        := ":"
    , TkSpace        := " "
    , TkBracketOpen  := "["
    , TkNumber       := "0"
    , TkComma        := ","
    , TkSpace        := " "
    , TkNumber       := "123"
    , TkBracketClose := "]"
    , TkSpace        := "\n"
    , TkBraceClose   := "}"
    , TkSpace        := "\n"
    ]
