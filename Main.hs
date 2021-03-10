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
  ( shiftL
  )
import Data.Functor
  ( void
  )
import Data.Ix
  ( inRange
  )
import Text.ParserCombinators.ReadP
  ( ReadP
  , char
  , get
  , munch1
  , readP_to_S
  , satisfy
  , sepBy
  , skipSpaces
  , string
  )
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

-- Generic show
show :: (Show a, IsString b) => a -> b
show = fromString . Show.show

type Parser = ReadP

(¤) :: a -> String -> Parser a
c ¤ s = c <$ string s

(<$>-) :: (a -> b) -> Parser a -> Parser b
f <$>- p = f <$> skipUntil p

(<*>-) :: Parser (a -> b) -> Parser a -> Parser b
f <*>- p = f <*> skipUntil p

skipUntil :: Parser a -> Parser a
skipUntil p = p <|> (get *> skipUntil p)

entryP :: Parser Entry
entryP = do
  void
    .  skipUntil
    $  string "\"nextDeliveryDays\""
    <* skipSpaces
    <* char ':'
    <* skipSpaces
  Entry <$> (char '[' *> skipSpaces *> elements <* skipSpaces <* char ']')
  where elements = sepBy jsonStringP (skipSpaces *> char ',' <* skipSpaces)

jsonStringP :: Parser String
jsonStringP = char '"' *> jString
 where
  jString = do
    optFirst <- optional jsonChar
    case optFirst of
      Nothing                              -> "" <$ char '"'
      Just first | not (isSurrogate first) -> (first :) <$> jString
      Just first                           -> do
        second <- jsonChar
        if isHighSurrogate first && isLowSurrogate second
          then (combineSurrogates first second :) <$> jString
          else empty

  isHighSurrogate = inRange (0xD800, 0xDBFF) . ord
  isLowSurrogate  = inRange (0xDC00, 0xDFFF) . ord
  isSurrogate     = liftA2 (||) isHighSurrogate isLowSurrogate

  combineSurrogates a b =
    chr $ ((ord a - 0xD800) `shiftL` 10) + (ord b - 0xDC00) + 0x10000

  jsonChar =
    ('"' ¤ "\\\"")
      <|> ('\\' ¤ "\\\\")
      <|> ('/' ¤ "\\/")
      <|> ('\b' ¤ "\\b")
      <|> ('\f' ¤ "\\f")
      <|> ('\n' ¤ "\\n")
      <|> ('\r' ¤ "\\r")
      <|> ('\t' ¤ "\\t")
      <|> (string "\\u" *> escapeUnicode)
      <|> satisfy (\c -> not (c == '\"' || c == '\\' || isControl c))
   where
    isControl c = c <= '\31'
    escapeUnicode =
      chr . fst . head . readHex <$> replicateM 4 (satisfy isHexDigit)

deliveryDayP :: Parser DeliveryDay
deliveryDayP = DeliveryDay <$>- weekDayP <*>- (read <$> digitsP) <*>- monthP

digitsP :: Parser String
digitsP = munch1 isDigit

weekDayP :: Parser DayOfWeek
weekDayP = do
  (Monday ¤ "mandag")
    <|> (Tuesday ¤ "tirsdag")
    <|> (Wednesday ¤ "onsdag")
    <|> (Thursday ¤ "torsdag")
    <|> (Friday ¤ "fredag")

monthP :: Parser Month
monthP =
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

runParser :: Parser a -> String -> Maybe a
runParser parser str = case readP_to_S parser str of
  []             -> Nothing
  ((res, _) : _) -> Just res

main :: IO ()
main = do
  now   <- getCurrentTime
  str   <- fetchData
  entry <- case runParser entryP str of
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
      <> (days >>= event now . runParser deliveryDayP)
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
