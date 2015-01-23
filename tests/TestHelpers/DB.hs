{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
module TestHelpers.DB
    ( runDB
    , createUser
    , createComment
    , createSubscription
    , createNotification
    , subscribeUser
    ) where

import Model
import Model.Subscription
import Model.UserComment
import Foundation
import Notification

import Control.Monad.IO.Class (liftIO)
import Data.Monoid ((<>))
import Data.Text (Text)
import Data.Time (getCurrentTime)
import Database.Persist
import Database.Persist.Sql (SqlBackend, SqlPersistM, runSqlPersistMPool)
import Text.Markdown
import Yesod.Test

import qualified Data.Text.Lazy as TL

type Example = YesodExample App

runDB :: SqlPersistM a -> Example a
runDB query = do
    pool <- fmap connPool getTestYesod
    liftIO $ runSqlPersistMPool query pool

createUser :: Text -> Example (Entity User)
createUser ident = do
    insertEntity User
        { userFirstName = "John" <> ident
        , userLastName  = "Smith"
        , userEmail     = "john-" <> ident <> "@gmail.com"
        , userPlugin    = "dummy"
        , userIdent     = ident
        }

createComment :: UserId -> Text -> Text -> TL.Text -> Example (Entity Comment)
createComment uid article thread body = do
    now <- liftIO getCurrentTime

    insertEntity Comment
        { commentUser = uid
        , commentThread = thread
        , commentArticleTitle = "title"
        , commentArticleURL = article
        , commentBody = Markdown body
        , commentCreated = now
        }

createSubscription :: Text -> Entity User -> Example ()
createSubscription name (Entity uid _) = runDB $ subscribe name uid

createNotification :: Text -> Text -> Entity User -> Example Notification
createNotification article thread u = do
    c <- createComment (entityKey u) article thread ""

    return $ NewComment $ UserComment c u

subscribeUser :: Text -> Text -> (Entity User) -> Example ()
subscribeUser article thread eu = do
    notification <- createNotification article thread eu

    createSubscription (notificationName notification) eu

insertEntity :: (PersistEntity e, PersistEntityBackend e ~ SqlBackend)
             => e -> Example (Entity e)
insertEntity e = do
    eid <- runDB $ insert e

    return $ Entity eid e
