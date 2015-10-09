{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Dota
import DotaParser

import Control.Lens
import Control.Monad
import Control.Monad.Reader

import Data.List (sortOn, intersperse)

import qualified Data.HashSet as S

import Data.Time

import System.Directory (doesFileExist)

import Text.Printf

import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.String (renderHtml)

isThrow :: Match -> Bool
isThrow = (< -15000) . minimum . (^. winnerAdvantage)

c9TeamId :: TeamId
c9TeamId = "1333179"

isc9 :: TeamDotabuff -> Bool
isc9 (TeamDotabuff tid _) = c9TeamId == tid


cacheFilename :: FilePath
cacheFilename = "c9.cache"

readCache :: IO [Match]
readCache = do
  exists <- doesFileExist cacheFilename
  if exists
    then read <$> readFile cacheFilename
    else return []

writeCache :: [Match] -> IO ()
writeCache = writeFile cacheFilename . show

getMatches :: IO [Match]
getMatches = do
  lostMatchIds <- getLostMatchIds c9TeamId
  cachedMatches <- readCache

  let cachedSet = S.fromList $ _matchId <$> cachedMatches
      cached = (`S.member` cachedSet)
      notCachedIds = filter (not . cached) lostMatchIds

  lostMatchData <- forM notCachedIds getMatchData
  let matches = lostMatchData ++ cachedMatches

  unless (null notCachedIds) $ writeCache matches

  return $ sortOn (^. matchTime) matches


data MetaData = MetaData { _localTimeZone :: TimeZone }
makeLenses ''MetaData

type Jizz = Reader MetaData
runJizz :: TimeZone -> Jizz a -> a
runJizz tz = flip runReader (MetaData tz)

printMatches :: IO ()
printMatches = do
  timezone <- getCurrentTimeZone
  matches <- getMatches
  forM_ matches $ \md -> printMatchInfo timezone md >> putStrLn (replicate 40 '-')
  printLastThrow matches

matchInfo :: Match -> Jizz [String]
matchInfo md = do
  toLocalTime <- views localTimeZone (to . utcToLocalTime)

  let match = md ^. matchId
      teams = md ^. matchTeams
      gs = md ^. winnerAdvantage

  return [ printf "Match %s: " match
         , printf "Time: %s" $ md ^. matchTime . toLocalTime . to show
         , printf "Winner: %s (%s)" (teams ^. winnerDotabuff . teamName) (teams ^. winner . to show)
         , printf "Winner advantage: [%6d, %6d]" (minimum gs) (maximum gs)
         , printf "It was %sa throw." $ if isThrow md then "" else "not " :: String
         ]

printMatchInfo :: Match -> Jizz (IO ())
printMatchInfo m = mapM_ putStrLn <$> matchInfo m

showThings :: String -> String -> Int -> String
showThings sing _ 1 = "1 " ++ sing
showThings _ prul n = printf "%d %s" n prul

printLastThrow :: [Match] -> IO ()
printLastThrow matches =
  case filter isThrow matches ^? _last of
    Nothing -> printf "They acutally never threw!!!"
    Just lastThrow -> reallyPrintLastThrow lastThrow

reallyPrintLastThrow :: Match -> IO ()
reallyPrintLastThrow lastThrow = do
  now <- getCurrentTime

  let throwTime = lastThrow ^. matchTime
      diffHours = fromEnum $ diffUTCTime now throwTime / (3600 * 1e12) :: Int
      (days, hours) = diffHours `divMod` 24

  printf "%s's last throw was " $ lastThrow ^. matchTeams . loserDotabuff . teamName
  printf "%s and %s ago " (showThings "day" "days" days) (showThings "hour" "hours" hours)
  printf "against %s.\n" $ lastThrow ^. matchTeams . winnerDotabuff . teamName

  printf "They threw away a %d gold lead.\n" $ lastThrow ^. winnerAdvantage . to minimum . to negate


genHtml :: IO ()
genHtml = do
  matches <- getMatches
  timeZone <- getCurrentTimeZone
  writeFile "c9.html" . renderHtml . htmlMatches timeZone $ matches

htmlMatches :: [Match] -> Jizz Html
htmlMatches matches = docTypeHtml <$> do
  timeZone <- view localTimeZone
  let css url = link ! rel "stylesheet" ! href url

  htmls <- mapM (fmap (li . p) . htmlMatch) $ reverse matches
  return $ do
    H.head $ do
      meta ! charset "utf-8"
      H.title "When did Cloud 9 last throw"
      css "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.5/css/bootstrap.min.css"
      css "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.5/css/bootstrap-theme.min.css"

    body $ H.div ! class_ "container" $ do
      h1 "When did Cloud 9 last throw"
      ol htmls
      script ! src "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.5/js/bootstrap.min.js" $ return ()

htmlMatch :: Match -> Jizz Html
htmlMatch m = sequence_ . intersperse br . fmap toHtml <$> matchInfo m


main :: IO ()
main = case 1 of
  1 -> printMatches
  2 -> genHtml
