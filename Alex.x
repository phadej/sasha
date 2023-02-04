{
module Sasha.Example.Alex (alexToken) where

import Data.Word (Word8)

import qualified Data.ByteString as BS

import Sasha.Example.Token

}

%encoding "latin1"

$ws = [\ \t\r\n]
$dq = [\"]

tokens :-

  $ws+              { TkSpace }
  [\{]              { TkBraceOpen }
  [\}]              { TkBraceClose }
  [\[]              { TkBracketOpen }
  [\]]              { TkBracketClose }
  [:]               { TkColon }
  [\,]              { TkComma }
  $dq (~$dq)+ $dq   { TkString }
  [0-9]+            { TkNumber }
  "true"            { TkTrue }
  "false"           { TkFalse }
  "null"            { TkNull }

{
type AlexInput = BS.ByteString

alexGetByte :: AlexInput -> Maybe (Word8, AlexInput)
alexGetByte = BS.uncons

alexToken :: BS.ByteString -> Maybe (Tk, BS.ByteString, BS.ByteString)
alexToken bs = case alexScan bs 0 of
    AlexEOF          -> Nothing
    AlexError _      -> Nothing
    AlexSkip bs' _   -> alexToken bs'
    AlexToken _ i tk -> case BS.splitAt i bs of
      (pfx, sfx) -> Just (tk, pfx, sfx)
}
