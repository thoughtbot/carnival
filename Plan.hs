module Plan
    ( PlanName(..)
    ) where

import Prelude
import Database.Persist.TH (derivePersistField)

data PlanName
    = Personal
    | Business
    | Enterprise
    deriving (Eq, Read, Show)

$(derivePersistField "PlanName")
