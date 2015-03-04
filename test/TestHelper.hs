{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module TestHelper
    ( withApp
    , module X
    ) where

-- Application code
import Model as X
import Model.Comment as X
import Model.User as X
import Model.UserComment as X
import Foundation as X

import Application (makeFoundation)
import Settings (Extra, parseExtra)

-- Useful libraries
import Control.Applicative as X
import Control.Monad as X
import Data.Text as X (Text)
import Data.Aeson as X
import Data.Monoid as X
import Database.Persist as X hiding (get, delete)

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (ReaderT, ask)
import Database.Persist.Sql
    ( SqlBackend
    , rawExecute
    , rawSql
    , unSingle
    , connEscapeName
    )
import Text.Shakespeare.Text (st)
import Yesod.Default.Config

-- Test framework
import Test.Hspec as X
import Yesod.Test as X

-- Test helpers
import TestHelpers.DB as X
import TestHelpers.Request as X
import TestHelpers.Response as X
import TestHelpers.Assertions as X
import TestHelpers.Auth as X

import qualified Data.Text as T

withApp :: SpecWith App -> Spec
withApp = before $ do
    foundation <- makeFoundation =<< testConfig
    wipeDB foundation
    return foundation

testConfig :: IO (AppConfig DefaultEnv Extra)
testConfig =
    Yesod.Default.Config.loadConfig
        $ (configSettings Testing)
        { csParseExtra = parseExtra }

wipeDB :: App -> IO ()
wipeDB app = runDBWithApp app $ do
    tables <- getTables
    sqlBackend <- ask

    let escapedTables = map (connEscapeName sqlBackend . DBName) tables
        query = "TRUNCATE TABLE " <> (T.intercalate ", " escapedTables)

    rawExecute query []

getTables :: MonadIO m => ReaderT SqlBackend m [Text]
getTables = do
    tables <- rawSql [st|
        SELECT table_name
        FROM information_schema.tables
        WHERE table_schema = 'public';
    |] []

    return $ map unSingle tables
