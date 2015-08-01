{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Dota where


import Control.Lens

import Data.Time (LocalTime)

type MatchId = String
type URL = String

data Side = Radiant | Dire deriving (Read, Show, Eq)

opponent :: Side -> Side
opponent Radiant = Dire
opponent Dire = Radiant

type TeamId = String
type TeamName = String
data TeamDotabuff = TeamDotabuff { _teamId :: TeamId
                                 , _teamName :: TeamName
                                 } deriving (Read, Show)
makeLenses ''TeamDotabuff

data MatchTeams = MatchTeams { _teamRadiant :: TeamDotabuff
                             , _teamDire :: TeamDotabuff
                             , _winner :: Side
                             } deriving (Read, Show)
makeLenses ''MatchTeams

matchTeam :: Side -> Lens' MatchTeams TeamDotabuff
matchTeam Radiant = teamRadiant
matchTeam Dire    = teamDire

flatmapLens :: (s -> ALens' s a) -> Lens' s a
flatmapLens g f x = (\h -> x & cloneLens l .~ h) <$> f (x ^. cloneLens l)
  where l = g x

flatmapLens' :: Getter s (ALens' s a) -> Lens' s a
flatmapLens' g = flatmapLens (^. g)

winnerDotabuff, loserDotabuff :: Lens' MatchTeams TeamDotabuff
winnerDotabuff = flatmapLens' (winner . to matchTeam)
loserDotabuff = flatmapLens' (winner . to opponent . to matchTeam)

type MatchTime = LocalTime
type GoldDifference = Integer

data Match = Match { _matchId :: MatchId
                   , _matchTeams :: MatchTeams
                   , _matchTime :: MatchTime
                   , _goldDifferences :: [GoldDifference]
                   } deriving (Read, Show)
makeLenses ''Match

winnerAdvantage :: Lens' Match [GoldDifference]
winnerAdvantage = flatmapLens' (matchTeams . winner . to (\w -> goldDifferences . mapping (signed w)))
  where signed Radiant = iso id id
        signed Dire    = iso negate negate
