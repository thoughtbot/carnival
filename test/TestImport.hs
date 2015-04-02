module TestImport
    ( withApp
    , runDB
    , authenticateAs
    , times
    , module X
    ) where

import Application           (makeFoundation)
import ClassyPrelude         as X
import Database.Persist      as X hiding (get)
import Database.Persist.Sql  (SqlPersistM, SqlBackend, runSqlPersistMPool, rawExecute, rawSql, unSingle, connEscapeName)
import Foundation            as X
import Model                 as X
import Test.Hspec            as X hiding
    ( expectationFailure
    , shouldBe
    , shouldSatisfy
    , shouldContain
    , shouldMatchList
    , shouldReturn
    )
import Test.Hspec.Expectations.Lifted as X
import Yesod.Default.Config2 (ignoreEnv, loadAppSettings)
import Yesod.Test.Extension  as X

import Settings as X
import Factories as X

runDB :: SqlPersistM a -> YesodExample App a
runDB query = do
    app <- getTestYesod
    liftIO $ runDBWithApp app query

runDBWithApp :: App -> SqlPersistM a -> IO a
runDBWithApp app query = runSqlPersistMPool query (appConnPool app)


withApp :: SpecWith App -> Spec
withApp = before $ do
    settings <- loadAppSettings
        ["config/test-settings.yml", "config/settings.yml"]
        []
        ignoreEnv
    foundation <- makeFoundation settings
    wipeDB foundation
    return foundation

-- This function will truncate all of the tables in your database.
-- 'withApp' calls it before each test, creating a clean environment for each
-- spec to run in.
wipeDB :: App -> IO ()
wipeDB app = do
    runDBWithApp app $ do
        tables <- getTables
        sqlBackend <- ask

        let escapedTables = map (connEscapeName sqlBackend . DBName) tables
            query = "TRUNCATE TABLE " ++ (intercalate ", " escapedTables)
        rawExecute query []

getTables :: MonadIO m => ReaderT SqlBackend m [Text]
getTables = do
    tables <- rawSql "SELECT table_name FROM information_schema.tables WHERE table_schema = 'public';" []
    return $ map unSingle tables

authenticateAs :: Entity User -> YesodExample App ()
authenticateAs (Entity _ u) = do
    testRoot <- fmap (appRoot . appSettings) getTestYesod

    let url = testRoot ++ "/auth/page/dummy"

    request $ do
        setMethod "POST"
        addPostParam "ident" $ userIdent u
        setUrl url

times :: Monad m => Int -> (Int -> m a) -> m [a]
times n = forM [1..n]
