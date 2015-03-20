module Plan
    ( PlanName(..)
    , plans
    , planFeatures
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
