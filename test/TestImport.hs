{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module TestImport
    ( withApp
    , runDB
    , authenticateAs
    , module X
    ) where

import Application

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (ReaderT, ask)
import Data.Monoid ((<>))
import Database.Persist.Sql
    ( SqlBackend
    , SqlPersistM
    , connEscapeName
    , rawExecute
    , rawSql
    , runSqlPersistMPool
    , unSingle
    )
import Text.Shakespeare.Text (st)
import Yesod.Default.Config

import qualified Data.Text as T

-- Re-exports
import Settings as X
import Model as X
import Foundation as X
import Factories as X
import Database.Persist as X hiding (get)
import Data.Text as X (Text)
import Test.Hspec as X hiding
    ( expectationFailure
    , shouldBe
    , shouldSatisfy
    , shouldContain
    , shouldMatchList
    , shouldReturn
    )
import Test.Hspec.Expectations.Lifted as X
import Yesod.Test.Extension as X

runDB :: SqlPersistM a -> YesodExample App a
runDB query = do
    app <- getTestYesod
    liftIO $ runDBWithApp app query

runDBWithApp :: App -> SqlPersistM a -> IO a
runDBWithApp app query = runSqlPersistMPool query (connPool app)

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
        query = "TRUNCATE TABLE " <> T.intercalate ", " escapedTables

    rawExecute query []

getTables :: MonadIO m => ReaderT SqlBackend m [Text]
getTables = do
    tables <- rawSql [st|
        SELECT table_name
        FROM information_schema.tables
        WHERE table_schema = 'public';
    |] []

    return $ map unSingle tables

authenticateAs :: Entity User -> YesodExample App ()
authenticateAs (Entity _ u) = do
    testRoot <- fmap (appRoot . settings) getTestYesod

    let url = testRoot `T.append` "/auth/page/dummy"

    request $ do
        setMethod "POST"
        addPostParam "ident" $ userIdent u
        setUrl url
