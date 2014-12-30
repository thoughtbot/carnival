module Main where

-- Libraries
import Database.Persist.Sql (runSqlPersistMPool)
import Test.Hspec (hspec, after_, before_)
import Yesod.Default.Config
import Yesod.Test

-- Application
import Import
import Application (makeFoundation)

-- Tests
import ApiTest
import NotificationTest

main :: IO ()
main = mapM_ run [apiSpecs, notificationSpecs]

run :: YesodSpec App -> IO ()
run spec = do
    foundation <- makeFoundation =<< testConfig

    hspec $
        before_ (cleanup foundation) $
        after_ (cleanup foundation) $
        yesodSpec foundation spec

    where
        cleanup :: App -> IO ()
        cleanup foundation = do
            let pool = connPool foundation

            flip runSqlPersistMPool pool $ do
                deleteWhere ([] :: [Filter Comment])
                deleteWhere ([] :: [Filter Subscription])
                deleteWhere ([] :: [Filter User])

testConfig :: IO (AppConfig DefaultEnv Extra)
testConfig =
    Yesod.Default.Config.loadConfig
        $ (configSettings Testing)
        { csParseExtra = parseExtra }
