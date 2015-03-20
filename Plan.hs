module Plan
    ( PlanName(..)
    , plans
    , planFeatures
    , planSiteCount
    , planCommentCount
    ) where

import Prelude

import Settings

import Database.Persist.TH (derivePersistField)
import Yesod.Core

data PlanName
    = Personal
    | Business
    | Enterprise
    deriving (Bounded, Enum, Eq, Read, Show)

$(derivePersistField "PlanName")

plans :: [PlanName]
plans = [minBound .. maxBound]

planFeatures :: PlanName -> WidgetT m IO ()
planFeatures Personal = $(widgetFile "plans/personal")
planFeatures Business = $(widgetFile "plans/business")
planFeatures Enterprise = $(widgetFile "plans/enterprise")

planSiteCount :: PlanName -> Int
planSiteCount Personal = 1
planSiteCount Business = 5
planSiteCount Enterprise = 10000

planCommentCount :: PlanName -> Int
planCommentCount Personal = 10
planCommentCount Business = 100
planCommentCount Enterprise = 100000
