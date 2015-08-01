{-# LANGUAGE OverloadedStrings #-}

module Main where

import Dota
import DotaParser

import Control.Lens
import Control.Monad

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


main :: IO ()
main = genHtml


printMatches :: IO ()
printMatches = do
  matches <- getMatches
  forM_ matches $ \md -> printMatchInfo md >> putStrLn (replicate 40 '-')
  printLastThrow matches

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

printLastThrow :: [Match] -> IO ()
printLastThrow matches =
  case filter isThrow matches ^? _last of
    Nothing -> printf "They acutally never threw!!!"
    Just lastThrow -> reallyPrintLastThrow lastThrow

reallyPrintLastThrow :: Match -> IO ()
reallyPrintLastThrow lastThrow = do
  now <- getCurrentTime
  timezone <- getCurrentTimeZone

  let throwTime = lastThrow ^. matchTime . to (localTimeToUTC timezone)
      diffHours = fromEnum $ diffUTCTime now throwTime / (3600 * 1e12) :: Int
      (days, hours) = diffHours `divMod` 24

  printf "%s's last throw was " $ lastThrow ^. matchTeams . loserDotabuff . teamName
  printf "%s and %s ago " (showThings "day" "days" days) (showThings "hour" "hours" hours)
  printf "against %s.\n" $ lastThrow ^. matchTeams . winnerDotabuff . teamName

  printf "They threw away a %d gold lead.\n" $ lastThrow ^. winnerAdvantage . to minimum . to negate


genHtml :: IO ()
genHtml = do
  matches <- getMatches
  writeFile "c9.html" . renderHtml . htmlMatches $ matches

htmlMatches :: [Match] -> Html
htmlMatches matches = docTypeHtml $ do
  H.head $ H.title "When did Cloud 9 last throw"
  body $ do
    h1 "When did Cloud 9 last throw"
    ol $ forM_ (reverse matches) (li . p . htmlMatch)

htmlMatch :: Match -> Html
htmlMatch md = sequence_ . intersperse br $ toHtml <$> content
    where match = md ^. matchId
          teams = md ^. matchTeams
          gs = md ^. winnerAdvantage

          content = [
              printf "Match %s: " match
            , printf "Time: %s" $ md ^. matchTime . to show
            , printf "Winner: %s (%s)" (teams ^. winnerDotabuff . teamName) (teams ^. winner . to show)
            , printf "Winner advantage: [%6d, %6d]" (minimum gs) (maximum gs)
            , printf "It was %sa throw." $ if isThrow md then "" else "not " :: String
            ] :: [String]
