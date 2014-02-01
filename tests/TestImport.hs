{-# LANGUAGE OverloadedStrings #-}
module TestImport
    ( module Yesod.Test
    , module Model
    , module Foundation
    , module Database.Persist
    , module Data.ByteString.Lazy
    , module Data.Text
    , runDB
    , Spec
    , Example
    , assertEqual'
    ) where

import Yesod.Test
import Database.Persist hiding (get)
import Database.Persist.Sql (SqlPersistM, runSqlPersistMPool)
import Control.Monad.IO.Class (liftIO)
import Data.ByteString.Lazy (ByteString)
import Data.Text (Text)

import Foundation
import Model

type Spec = YesodSpec App
type Example = YesodExample App

runDB :: SqlPersistM a -> Example a
runDB query = do
    pool <- fmap connPool getTestYesod
    liftIO $ runSqlPersistMPool query pool

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
