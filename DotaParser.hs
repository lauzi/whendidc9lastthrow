{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}

module DotaParser ( getLostMatchIds
                   , getMatchData) where

import Dota

import Control.Lens hiding (element, elements)
import Data.Text.Lens
import Data.List.Lens
import Data.Aeson.Lens
import Text.Taggy.Lens

import Control.Monad (guard)

import Data.HashMap.Strict (HashMap)
import Data.Char (isDigit)
import Data.Monoid
import qualified Data.Text as T

import Network.Wreq (get, responseBody)
import Data.Text.Lazy.Encoding (decodeUtf8)

import Data.Time.Format (parseTimeOrError, defaultTimeLocale)

import Debug.Trace

classIs :: T.Text -> Fold (HashMap T.Text T.Text) ()
classIs x = ix "class" . only x


goldDifferencesF :: Fold String Integer
goldDifferencesF = dive . traverse . hasGoldHeader . body
  where hasGoldHeader = filtered $ \x -> "Gold" == x ^. nth 0 . _String
        body = _Array . _tail . traverse . _Integer
        dive = key "difference" . _Array

matchIdF :: Fold Element String
-- to :: (s -> a) -> Getter s a
-- ix _ :: Traversal' s a
matchIdF = to eltAttrs . ix "href" . to parse
  where parse = T.unpack . T.reverse . T.takeWhile isDigit . T.reverse

lostMatches :: HasElements a => Fold a Element
lostMatches = elements . allAttributed (classIs "lost")

allShit :: HasElements a => Fold a Element
allShit = elements . to universe . traverse

allContent :: HasElements a => Fold a T.Text
allContent = allShit . contents

winnerF :: HasElements a => Fold a Side
winnerF = allContent . unpacked . suffixed " Victory" . to read

graphData :: HasElements a => Fold a String
graphData = allContent . unpacked . prefixed "var graphData = " . to (takeWhile (/= ';'))

dateF :: HasElements a => Fold a String
dateF = elements . allAttributed (classIs "header-content") . allShit . attr "datetime" . _Just . unpacked

{-
child :: T.Text -> Traversal' Element Element
child tag = elements . named (only tag)

htmlTitle :: Fold Element T.Text
htmlTitle = child "html" . child "head" . child "title" . children' . traverse . content
  where children' = Text.Taggy.Lens.children
--}


takeBackWhile :: (a -> Bool) -> [a] -> [a]
takeBackWhile p = reverse . takeWhile p . reverse


teamDotabuff :: Element -> Maybe TeamDotabuff
teamDotabuff header = do
  let allNodes = header ^.. allShit
  tid <- allNodes ^? traverse . attr "href" . _Just . unpacked . to (takeBackWhile isDigit)
  tname <- allNodes ^? traverse . attr "alt" . _Just . unpacked
  return $ TeamDotabuff tid tname

parseGraphPage :: Node -> Maybe [GoldDifference]
parseGraphPage page = do
  -- matchid <- page ^? element . htmlTitle . unpacked . to (filter isDigit)
  let golds = page ^.. graphData . goldDifferencesF
  guard $ not (null golds)
  return golds

matchOverview :: Node -> Maybe (MatchTeams, MatchTime)
matchOverview page = do
  teamResults <- page ^? allAttributed (classIs "team-results")
  dateString <- page ^? dateF
  let date = parseTimeOrError True defaultTimeLocale "%FT%T%z" dateString
  victor <- page ^? winnerF

  let header team = elements . attributed (classIs team) . elements . named (only "header")
  radiant <- teamDotabuff =<< teamResults ^? header "radiant"
  dire    <- teamDotabuff =<< teamResults ^? header "dire"

  return (MatchTeams radiant dire victor, date)


getPage :: String -> IO Node
getPage url = do -- trace ("getting page " <> url) $ do
  res <- get url
  case res ^? responseBody . to decodeUtf8 . html of
    Nothing -> error $ mconcat ["[Error] wreq could not get/parse page \"", url, "\""]
    Just node -> return node


dotabuffTeamPage :: TeamId -> URL
dotabuffTeamPage tid = mconcat ["http://www.dotabuff.com/esports/teams/", tid,
                                "?date=&hero=&region=&duration=30-&faction="]

dotabuffOverviewPage :: MatchId -> URL
dotabuffOverviewPage match = mconcat ["http://www.dotabuff.com/matches/", match]

yaspGraphPage :: MatchId -> URL
yaspGraphPage match = mconcat ["http://yasp.co/matches/", match, "/graphs"]


getLostMatchIds :: TeamId -> IO [MatchId]
getLostMatchIds tid = do
  page <- getPage $ dotabuffTeamPage tid
  return $ page ^.. lostMatches . matchIdF

getMatchOverview :: MatchId -> IO (MatchTeams, MatchTime)
getMatchOverview match = do
  page <- getPage $ dotabuffOverviewPage match
  let Just matchResult = matchOverview page
  return matchResult

getGoldDifferences :: MatchId -> IO [GoldDifference]
getGoldDifferences match = do
  page <- getPage $ yaspGraphPage match
  let Just gds = parseGraphPage page
  return gds

getMatchData :: MatchId -> IO Match
getMatchData match = do
    (matchTeams', matchTime') <- getMatchOverview match
    gds <- getGoldDifferences match
    return $ Match match matchTeams' matchTime' gds
