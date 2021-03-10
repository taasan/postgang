{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main
  )
where

import Prelude hiding
  ( show
  )

import Control.Applicative
  ( Alternative (..)
  , liftA2
  , optional
  )
import Control.Monad
  ( replicateM
  )

import Data.Char
  ( chr
  , isDigit
  , isHexDigit
  , ord
  )
import Data.List
  ( intercalate
  )
import Data.Maybe
  ( catMaybes
  , isNothing
  )
import Data.String
  ( IsString (..)
  )
import Data.Time
  ( UTCTime (utctDay)
  , defaultTimeLocale
  , formatTime
  , getCurrentTime
  , toGregorian
  )

import Numeric
  ( readHex
  )

import System.Environment
  ( getArgs
  )
import System.IO
  ( IOMode (WriteMode)
  , hPutStr
  , withFile
  )
import System.Process
  ( proc
  , readCreateProcess
  )

import Data.Bits
  ( Bits (shiftL)
  )
import Data.Functor
  ( void
  )
import Data.Ix
  ( inRange
  )
import qualified Text.ParserCombinators.ReadP as P
import Text.Printf
  ( printf
  )
import qualified Text.Show as Show

newtype Entry =
  Entry [String]
  deriving (Show)

data DayOfWeek
  = Monday
  | Tuesday
  | Wednesday
  | Thursday
  | Friday
  deriving (Show, Eq, Enum)

data Month
  = January
  | February
  | March
  | April
  | May
  | June
  | July
  | August
  | September
  | October
  | November
  | December
  deriving (Show, Eq, Enum)

data DeliveryDay =
  DeliveryDay
    { day :: DayOfWeek
    , dayNum :: Int
    , month :: Month
    }
  deriving (Show)

data JsonValue
  = JsonNull
  | JsonBool Bool
  | JsonNumber Double
  | JsonString String
  | JsonArray [JsonValue]
  | JsonObject [(String, JsonValue)]
  deriving (Show, Eq)

entryP :: Parser Entry
entryP = do
  void . skipUntil $ P.string "\"nextDeliveryDays\"" <* ws <* P.char ':' <* ws
  Entry <$> (P.char '[' *> ws *> elements <* ws <* P.char ']')
 where
  elements :: Parser [String]
  elements = P.sepBy jsonStringP (ws *> P.char ',' <* ws)

-- Generic show
show :: (Show a, IsString b) => a -> b
show = fromString . Show.show

type Parser = P.ReadP

ws :: P.ReadP ()
ws = P.skipSpaces

jsonStringP :: Parser String
jsonStringP = P.char '"' *> jString
 where
  jString = do
    optFirst <- optional char
    case optFirst of
      Nothing                              -> "" <$ P.char '"'
      Just first | not (isSurrogate first) -> (first :) <$> jString
      Just first                           -> do
        second <- char
        if isHighSurrogate first && isLowSurrogate second
          then (combineSurrogates first second :) <$> jString
          else empty

  isHighSurrogate = inRange (0xD800, 0xDBFF) . ord
  isLowSurrogate  = inRange (0xDC00, 0xDFFF) . ord
  isSurrogate     = liftA2 (||) isHighSurrogate isLowSurrogate

  combineSurrogates a b =
    chr $ ((ord a - 0xD800) `shiftL` 10) + (ord b - 0xDC00) + 0x10000

  char =
    ('"' ¤ "\\\"")
      <|> ('\\' ¤ "\\\\")
      <|> ('/' ¤ "\\/")
      <|> ('\b' ¤ "\\b")
      <|> ('\f' ¤ "\\f")
      <|> ('\n' ¤ "\\n")
      <|> ('\r' ¤ "\\r")
      <|> ('\t' ¤ "\\t")
      <|> (P.string "\\u" *> escapeUnicode)
      <|> P.satisfy (\c -> not (c == '\"' || c == '\\' || isControl c))
   where
    isControl c = c <= '\31'
    escapeUnicode =
      chr . fst . head . readHex <$> replicateM 4 (P.satisfy isHexDigit)

parseDeliveryDay :: Parser DeliveryDay
parseDeliveryDay =
  DeliveryDay <$>- parseWeekDay <*>- (read <$> digits) <*>- parseMonth

digits :: Parser String
digits = P.munch1 isDigit

(<$>-) :: (a -> b) -> Parser a -> Parser b
f <$>- p = f <$> skipUntil p

(<*>-) :: Parser (a -> b) -> Parser a -> Parser b
f <*>- p = f <*> skipUntil p

skipUntil :: Parser a -> Parser a
skipUntil p = p <|> (P.get *> skipUntil p)

parseWeekDay :: Parser DayOfWeek
parseWeekDay = do
  (Monday ¤ "mandag")
    <|> (Tuesday ¤ "tirsdag")
    <|> (Wednesday ¤ "onsdag")
    <|> (Thursday ¤ "torsdag")
    <|> (Friday ¤ "fredag")

parseMonth :: Parser Month
parseMonth =
  (January ¤ "januar")
    <|> (February ¤ "februar")
    <|> (March ¤ "mars")
    <|> (April ¤ "april")
    <|> (May ¤ "mai")
    <|> (June ¤ "juni")
    <|> (July ¤ "juli")
    <|> (August ¤ "august")
    <|> (September ¤ "september")
    <|> (October ¤ "oktober")
    <|> (November ¤ "november")
    <|> (December ¤ "desember")

(¤) :: a -> String -> Parser a
c ¤ s = c <$ P.string s

parse :: Parser a -> String -> Maybe a
parse parser str = case P.readP_to_S parser str of
  []             -> Nothing
  ((res, _) : _) -> Just res

main :: IO ()
main = do
  now   <- getCurrentTime
  str   <- fetchData
  entry <- case parse entryP str of
    Just value -> pure value
    _          -> error $ "Invalid data: " <> str
  argv <- getArgs
  let output = ical now entry
  if any isNothing output
    then error $ "Invalid input" <> show entry <> "\n" <> show output
    else do
      let fileName = case argv of
            ("-" : _) -> "/dev/stdout"
            (x   : _) -> x
            _         -> "/dev/stdout"
      withFile fileName WriteMode $ \h -> do
        (hPutStr h . intercalate "\r\n") $ catMaybes output
 where
  ical :: UTCTime -> Entry -> [Maybe String]
  ical now (Entry days) =
    (Just <$> preamble)
      <> (days >>= event now . parse parseDeliveryDay)
      <> (Just <$> ["END:VCALENDAR", ""])
  event :: UTCTime -> Maybe DeliveryDay -> [Maybe String]
  event now (Just dd@(DeliveryDay dayName d m)) =
    let
      year = thisYear + if thisMonth == 12 && m /= December then 1 else 0
      (thisYear, thisMonth, _) = (toGregorian . utctDay) now
    in
      Just
        <$> [ "BEGIN:VEVENT"
            , "UID:" <> show dd
            , "SUMMARY:Posten kommer " <> show dayName
            , printf "DTSTART:%d%02d%02d" year (fromEnum m + 1) d
            , "DURATION:P1D"
            , "DTSTAMP:" <> formatTime defaultTimeLocale "%C%y%m%dT%H%M%S" now
            , "END:VEVENT"
            ]
  event _ _ = []
  preamble :: [String]
  preamble =
    [ "BEGIN:VCALENDAR"
    , "VERSION:2.0"
    , "PRODID:-//Aasan//Aasan Postgang 1.0//EN"
    , "CALSCALE:GREGORIAN"
    , "METHOD:PUBLISH"
    ]

  fetchData :: IO String
  fetchData = readCreateProcess
    (proc
      "curl"
      [ "-sSL"
      , "https://www.posten.no/levering-av-post-2020/_/component/main/1/leftRegion/1?postCode=7530"
      , "-H"
      , "x-requested-with: XMLHttpRequest"
      ]
    )
    ""
