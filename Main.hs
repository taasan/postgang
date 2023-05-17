{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module Main
  ( main
  )
where

#ifndef CURRENT_PACKAGE_VERSION
#define CURRENT_PACKAGE_VERSION "[unknown]"
#endif

import Prelude

import Control.Applicative
  ( Alternative (..)
  , liftA2
  , liftA3
  , optional
  )
import Control.Monad
  ( join
  , replicateM
  , when
  )

import Data.Char
  ( chr
  , isDigit
  , isHexDigit
  , ord
  )
import Data.Maybe
  ( catMaybes
  , fromJust
  , isNothing
  )
import Data.Time
  ( UTCTime
  , DayOfWeek(..)
  , dayOfWeek
  , defaultTimeLocale
  , formatTime
  , fromGregorian
  , getCurrentTime
  , toGregorian
  )
import Data.Time.Calendar
  ( Day
  )
import Data.Time.Format
  ( readPTime
  )

import Numeric
  ( readHex
  )

import GitHash

import System.Console.GetOpt
  ( ArgDescr (..)
  , ArgOrder (..)
  , OptDescr (..)
  , getOpt
  , usageInfo
  )
import System.Environment
  ( getArgs
  , getEnv
  , getProgName
  )
import System.IO
  ( IOMode (..)
  , Newline (CRLF)
  , NewlineMode (..)
  , hClose
  , hPutStr
  , hPutStrLn
  , hSetEncoding
  , hSetNewlineMode
  , openFile
  , stderr
  , stdin
  , stdout
  , utf8
  )
import System.Process
  ( readProcessWithExitCode
  )

import Data.Bits
  ( shiftL
  )
import Data.Foldable
  ( traverse_
  )
import Data.Function
  ( (&)
  )
import Data.Functor
  ( void
  )
import Data.Ix
  ( inRange
  )
import Network.HostName
  ( getHostName
  )
import System.Exit
  ( ExitCode (..)
  , exitFailure
  , exitSuccess
  , exitWith
  )
import TH
import Text.ParserCombinators.ReadP
  ( ReadP
  , char
  , eof
  , get
  , munch1
  , readP_to_S
  , satisfy
  , sepBy
  , skipSpaces
  , string
  )
import Text.Printf
  ( PrintfType
  , hPrintf
  , printf
  )

type StringT = String

newtype Entry =
  Entry [StringT]
  deriving (Show)

showDow :: DayOfWeek -> String
showDow Monday    = "mandag"
showDow Tuesday   = "tirsdag"
showDow Wednesday = "onsdag"
showDow Thursday  = "torsdag"
showDow Friday    = "fredag"
showDow Saturday  = "lørdag"
showDow Sunday    = "søndag"

type Parser = ReadP

newtype PostalCode
  = PostalCode Int

instance Show PostalCode where
  show (PostalCode p) = printf "%04d" p

postalCodeP :: Parser PostalCode
postalCodeP = PostalCode . read <$> replicateM 4 (satisfy isDigit)

(¤) :: a -> StringT -> Parser a
c ¤ s = c <$ string s

(<$>-) :: (a -> b) -> Parser a -> Parser b
g <$>- p = g <$> skipUntil p

(<*>-) :: Parser (a -> b) -> Parser a -> Parser b
g <*>- p = g <*> skipUntil p

skipUntil :: Parser a -> Parser a
skipUntil p = p <|> (get *> skipUntil p)

entryP :: Parser Entry
entryP = do
  (void . skipUntil)
    $  string "\"delivery_dates\""
    <* skipSpaces
    <* char ':'
    <* skipSpaces
  Entry <$> (char '[' *> skipSpaces *> elements <* skipSpaces <* char ']')
  where elements = sepBy jsonStringP (skipSpaces *> char ',' <* skipSpaces)

jsonStringP :: Parser StringT
jsonStringP = char '"' *> jString
 where
  jString = do
    optional jsonChar >>= \case
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
    isControl c = c < '\x20'
    escapeUnicode =
      chr . fst . head . readHex <$> replicateM 4 (satisfy isHexDigit)

deliveryDayP :: Parser Day
deliveryDayP = readPTime False defaultTimeLocale "%Y-%m-%d"

digitsP :: Parser StringT
digitsP = munch1 isDigit

runParser :: Parser a -> StringT -> Maybe a
runParser parser str = case readP_to_S parser str of
  []             -> Nothing
  ((res, _) : _) -> Just res

f :: Alternative f => (t -> Maybe a) -> t -> f a
f g x = case g x of
  Just res -> pure res
  _        -> empty

-- Options
data Options = Options
    { optOutput :: Maybe FilePath
    , optPostalCode :: Maybe PostalCode
    , optShowHelp :: Bool
    , optShowVersion :: Bool
    } deriving Show

defaultOptions :: Options
defaultOptions = Options { optOutput      = Nothing
                         , optPostalCode  = Nothing
                         , optShowHelp    = False
                         , optShowVersion = False
                         }

options :: [OptDescr (Options -> Options)]
options =
  [ Option ['o']
           ["output"]
           (ReqArg (\val opts -> opts { optOutput = Just val }) "FILE")
           "Path of output file"
  , Option
    ['c']
    ["code"]
    (ReqArg (\val opts -> opts { optPostalCode = code val }) "CODE")
    "Postal code. If not specified, read input from stdin instead of fetching from posten.no"
  , Option ['h']
           ["help"]
           (NoArg (\opts -> opts { optShowHelp = True }))
           "Show this help message"
  , Option ['V']
           ["version"]
           (NoArg (\opts -> opts { optShowVersion = True }))
           "Show version information"
  ]
 where
  code val = case runParser (postalCodeP <* eof) val of
    Nothing -> error $ "Invalid code: " <> show val
    x       -> x

main :: IO ()
main = do
  traverse_ (`hSetEncoding` utf8) [stdout, stderr, stdin]
  prog <- getProgName
  (Options outFile postalCode showHelp showVersionO, rest) <- opts
  when
    (rest /= [])
    do
      hPrintf stderr "Superfluous arguments: %s" (show rest :: StringT)
      hPutStrLn stderr ""
      hPutStrLn stderr $ showUsage prog
      exitFailure

  when
    showHelp
    do
      showUsage prog
      exitSuccess

  when
    showVersionO
    do
      printf' "Build date" $$tNow
      putStrLn ""
      traverse_ putStrLn gitVersionInfo
      exitSuccess

  (exitCode, response, errResponse) <- fetchData postalCode
  if exitCode /= ExitSuccess
    then do
      exitWithError exitCode "Failed to fetch data" errResponse
    else if response == ""
      then do
        exitWithError (ExitFailure 1) "Got empty data" errResponse
      else do
        pure ()

  outputLines <- liftA3 (ical postalCode)
                        getHostName
                        getCurrentTime
                        (f (runParser entryP) response)

  when (any isNothing outputLines)
    $  error
    $  "Invalid input"
    <> "\n"
    <> show outputLines

  let icalOutput = unlines $ catMaybes outputLines
  handle <- if isNothing outFile || outFile == Just "-"
    then pure stdout
    else openFile (fromJust outFile) WriteMode
  hSetNewlineMode handle (NewlineMode CRLF CRLF)
  hPutStr handle icalOutput
  hClose handle
 where
  exitWithError :: ExitCode -> String -> String -> IO ()
  exitWithError exitCode reason errResponse = do
    hPutStrLn stderr $ printf' "ERROR" reason
    traverse_ (hPutStrLn stderr) gitVersionInfo
    hPutStrLn stderr errResponse
    exitWith exitCode

  showUsage :: PrintfType r => StringT -> r
  showUsage = printf "Usage: %s" . flip usageInfo options

  ical :: Maybe PostalCode -> StringT -> UTCTime -> Entry -> [Maybe StringT]
  ical code hostname now (Entry days) =
    (Just <$> preamble code)
      <> (days >>= event code hostname now . runParser deliveryDayP)
      <> (Just <$> ["END:VCALENDAR"])

  event
    :: Maybe PostalCode
    -> StringT
    -> UTCTime
    -> Maybe Day
    -> [Maybe StringT]
  event code hostname now (Just day) =
    let
      (year, m, d) = toGregorian day
      dtstart                  = fromGregorian year (fromEnum m + 1) d
      dtend                    = succ dtstart
      icalDate field =
        formatTime defaultTimeLocale $ field <> ";VALUE=DATE:%C%y%m%d"
      code' = case code of
        Just x -> show x ++ ": "
        _      -> ""
    in
      Just
        <$> [ "BEGIN:VEVENT"
            , printf "UID:postgang-%s@%s"
                     (formatTime defaultTimeLocale "%C%y%m%d" dtstart)
                     hostname
            , "URL:https://www.posten.no/levering-av-post/"
            , printf "SUMMARY:%sPosten kommer %s %d." code' (showDow . dayOfWeek $ day) d
            , icalDate "DTSTART" dtstart
            , icalDate "DTEND"   dtend
            , formatTime defaultTimeLocale "DTSTAMP:%C%y%m%dT%H%M%SZ" now
            , "TRANSP:TRANSPARENT"
            , "END:VEVENT"
            ]
  event _ _ _ _ = []

  preamble :: Maybe PostalCode -> [StringT]
  preamble mCode =
    [ "BEGIN:VCALENDAR"
    , "VERSION:2.0"
    , printf "PRODID:-//Aasan//Aasan Postgang %s%s//EN" code
      $ (escapeIcalString . giTag) gi
    , "CALSCALE:GREGORIAN"
    , "METHOD:PUBLISH"
    ]
   where
    code = case mCode of
      Just c -> show c <> "@"

      _      -> ""

  fetchData :: Maybe PostalCode -> IO (ExitCode, StringT, StringT)
  fetchData Nothing     = (ExitSuccess, , "") <$> getContents
  fetchData (Just code) = do
    apiUid <- getEnv "POSTGANG_API_UID"
    apiKey <- getEnv "POSTGANG_API_KEY"
    readProcessWithExitCode
      "curl"
      [ "-sSL"
      , "--retry"
      , "5"
      , "--fail"
      , "https://api.bring.com/address/api/no/postal-codes/"
        <> show code
        <> "/mailbox-delivery-dates"
      , "-H"
      , "X-Mybring-API-Uid: " <> apiUid
      , "-H"
      , "X-Mybring-API-Key: " <> apiKey
      ]
      ""

  opts = do
    argv <- getArgs
    prog <- getProgName
    let header = showUsage prog
    case getOpt RequireOrder options argv of
      (o, n, []) -> return (foldl (&) defaultOptions o, n)
      (_, _, errs) ->
        ioError (userError (join errs <> usageInfo header options))

  gitVersionInfo
    | giDirty gi
    = [ printf' "Branch"
          $  giBranch gi
          <> "@"
          <> giHash gi
          <> " (uncommitted files present)"
      ]
    | otherwise
    = [ printf' "Version" version
      , ""
      , printf' "Tag" $ giTag gi
      , printf' "Branch" $ giBranch gi
      , printf' "Date" $ giCommitDate gi
      , printf' "Message" $ giCommitMessage gi
      ]

  printf' :: PrintfType r => StringT -> r
  printf' = printf "%-12s: %s"

  commit :: StringT
  commit = printf "%s@%s" (giBranch gi) (giHash gi)

  gi     = $$tGitInfoCwd

  escapeIcalString :: StringT -> StringT
  escapeIcalString s = s >>= escapeIcalChar

  escapeIcalChar :: Char -> StringT
  escapeIcalChar c | c == '\\' || c == ';' || c == ',' = ['\\', c]
  escapeIcalChar '\n' = "\\n"
  escapeIcalChar c    = [c]

  version :: StringT
  version = CURRENT_PACKAGE_VERSION
