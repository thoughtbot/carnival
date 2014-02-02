module Model where

import Prelude
import Yesod
import Data.Text (Text)
import Database.Persist.Quasi
import Control.Applicative
import Control.Monad

-- You can define all of your database entities in the entities file.
-- You can find more information on persistent and how to declare entities
-- at:
-- http://www.yesodweb.com/book/persistent/
share [mkPersist sqlOnlySettings, mkMigrate "migrateAll"]
    $(persistFileWith lowerCaseSettings "config/models")

instance ToJSON (Entity Comment) where
    toJSON (Entity cid c) = object
        [ "id"     .= (String $ toPathPiece cid)
        , "thread" .= commentThread c
        , "body"   .= commentBody c
        ]

instance FromJSON Comment where
    parseJSON (Object v) = Comment
        <$> v .: "thread"
        <*> v .: "body"

    parseJSON _ = mzero
