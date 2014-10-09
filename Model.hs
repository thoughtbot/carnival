module Model where

import Prelude
import Yesod
import Data.Text (Text)
import Data.Time (UTCTime)
import Database.Persist.Quasi
import Data.Typeable (Typeable)
import Text.Markdown (Markdown(..))
import Yesod.Text.Markdown ()

type Validated a = Either [Text] a
type Validation a = a -> Validated a

share [mkPersist sqlSettings, mkMigrate "migrateAll"]
    $(persistFileWith lowerCaseSettings "config/models")
