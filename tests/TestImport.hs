{-# LANGUAGE OverloadedStrings #-}
module TestImport
    ( module Yesod.Test
    , module Model
    , module Foundation
    , module Database.Persist
    , module Data.Aeson
    , module Data.ByteString.Lazy
    , module Data.Text
    , runDB
    , Spec
    , Example
    , getWithParams
    , assertEqual'
    , bodyEquals'
    , insertComments
    , clearComments
    ) where

import Yesod (RedirectUrl, Yesod)
import Yesod.Test
import Database.Persist hiding (get)
import Database.Persist.Sql (SqlPersistM, runSqlPersistMPool)
import Control.Monad.IO.Class (liftIO)

import Data.Aeson
import Data.ByteString.Lazy (ByteString)
import Data.Text (Text)

import qualified Data.ByteString.Lazy.Char8 as BS

import Foundation
import Model

type Spec = YesodSpec App
type Example = YesodExample App

runDB :: SqlPersistM a -> Example a
runDB query = do
    pool <- fmap connPool getTestYesod
    liftIO $ runSqlPersistMPool query pool

getWithParams :: (RedirectUrl site url, Yesod site)
              => url -> [(Text, Text)] -> YesodExample site ()
getWithParams url params = request $ do
    setMethod  "GET"
    mapM (\(k, v) -> addGetParam k v) params
    setUrl url

-- | Like @assertEqual@ but no message argument.
--
--   > assertEqual' expected actual
--
assertEqual' :: (Eq a, Show a) => a -> a -> YesodExample site ()
assertEqual' expected actual = assertEqual msg expected actual

    where
        msg :: String
        msg = unlines
            [ "Values were not equal."
            , "  Expected: " ++ show expected
            , "    Actual: " ++ show actual
            ]

-- | Like @bodyEquals@ but taking a ByteString argument
bodyEquals' :: ByteString -> YesodExample site ()
bodyEquals' = bodyEquals . BS.unpack

-- | Clear the comments database and add the given comments. Pass the
--   empty list to just clear.
insertComments :: [Comment] -> Example ()
insertComments comments = runDB $ do
    deleteWhere ([] :: [Filter Comment])
    mapM_ insert comments

-- | Clears the comments table.
clearComments :: Example ()
clearComments = insertComments []

