module Plan
    ( Plan(..)
    , plans
    , planFeatures
    , planSiteCount
    , planCommentCount
    ) where

import Prelude

import Settings

import Database.Persist.TH (derivePersistField)
import Yesod.Core

data Plan
    = Personal
    | Business
    | Enterprise
    deriving (Bounded, Enum, Eq, Read, Show)

$(derivePersistField "Plan")

plans :: [Plan]
plans = [minBound .. maxBound]

planFeatures :: Plan -> WidgetT m IO ()
planFeatures Personal = $(widgetFile "plans/personal")
planFeatures Business = $(widgetFile "plans/business")
planFeatures Enterprise = $(widgetFile "plans/enterprise")

planSiteCount :: Plan -> Int
planSiteCount Personal = 1
planSiteCount Business = 5
planSiteCount Enterprise = 10000

planCommentCount :: Plan -> Int
planCommentCount Personal = 10
planCommentCount Business = 100
planCommentCount Enterprise = 100000
