module Main where

import Dota
import DotaParser

import Control.Lens
import Control.Monad

import Data.List (sortOn)

import qualified Data.HashSet as S

import Data.Time

import System.Directory (doesFileExist)

import Text.Printf


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


main :: IO ()
main = mainCached

mainCached :: IO ()
mainCached = do
  matches <- getMatches
  forM_ matches $ \md -> printMatchInfo md >> putStrLn (replicate 40 '-')
  printThrow matches

printMatchInfo :: Match -> IO ()
printMatchInfo md = do
    let match = md ^. matchId
        teams = md ^. matchTeams
    printf "Match %s: \n" match
    printf "Time: %s\n" $ md ^. matchTime . to show
    printf "Winner: %s (%s)\n" (teams ^. winnerDotabuff . teamName) (teams ^. winner . to show)
    let gs = md ^. winnerAdvantage
    printf "Winner advantage: [%6d, %6d]\n" (minimum gs) (maximum gs)
    printf "It was %sa throw.\n" $ if isThrow md then "" else "not " :: String

showThings :: String -> String -> Int -> String
showThings sing _ 1 = "1 " ++ sing
showThings _ prul n = printf "%d %s" n prul

printThrow :: [Match] -> IO ()
printThrow matches =
  case filter isThrow matches ^? _last of
    Nothing -> printf "This team acutally never threw!!!"
    Just lastThrow -> reallyPrintThrow lastThrow

reallyPrintThrow :: Match -> IO ()
reallyPrintThrow lastThrow = do
  now <- getCurrentTime
  timezone <- getCurrentTimeZone

  printf "%s's last throw was " $ lastThrow ^. matchTeams . loserDotabuff . teamName

  let throwTime = lastThrow ^. matchTime . to (localTimeToUTC timezone)
      diffHours = fromEnum $ diffUTCTime now throwTime / (3600 * 1e12) :: Int
      (days, hours) = diffHours `divMod` 24

  printf "%s and %s ago " (showThings "day" "days" days) (showThings "hour" "hours" hours)

  printf "against %s.\n" $ lastThrow ^. matchTeams . winnerDotabuff . teamName

  printf "They threw away a %d gold lead.\n" $ lastThrow ^. winnerAdvantage . to minimum . to negate


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
