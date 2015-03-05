{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
module TestHelpers.DB
    ( runDB
    , runDBWithApp
    , buildSite
    , createSite
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
    app <- getTestYesod
    liftIO $ runDBWithApp app query

runDBWithApp :: App -> SqlPersistM a -> IO a
runDBWithApp app query = runSqlPersistMPool query (connPool app)

buildSite :: Site
buildSite = Site
    { siteName = "site-name"
    , siteBaseUrl = "http://localhost:4000"
    , siteRssAuthor = "Rss Author"
    , siteRssTitle = "Rss Feed"
    , siteRssDescription = "Rss Feed"
    , siteRssLanguage = "en-us"
    }

createSite :: Example (Entity Site)
createSite = insertEntity buildSite

createUser :: Text -> Example (Entity User)
createUser ident =
    insertEntity User
        { userName      = "John Smith (" <> ident <> ")"
        , userEmail     = "john-" <> ident <> "@gmail.com"
        , userPlugin    = "dummy"
        , userIdent     = ident
        }

createComment :: UserId -> SiteId -> Text -> Text -> TL.Text -> Example (Entity Comment)
createComment uid siteId article thread body = do
    now <- liftIO getCurrentTime

    insertEntity Comment
        { commentUser = uid
        , commentSite = siteId
        , commentThread = thread
        , commentArticleTitle = "title"
        , commentArticleURL = article
        , commentArticleAuthor = "John Smith"
        , commentBody = Markdown body
        , commentCreated = now
        }

createSubscription :: Text -> Entity User -> Example ()
createSubscription name (Entity uid _) = runDB $ subscribe name uid

createNotification :: SiteId -> Text -> Text -> Entity User -> Example Notification
createNotification siteId article thread u = do
    c <- createComment (entityKey u) siteId article thread ""

    fmap NewComment $ runDB $ buildUserComment siteId c u

subscribeUser :: SiteId -> Text -> Text -> Entity User -> Example ()
subscribeUser siteId article thread eu = do
    notification <- createNotification siteId article thread eu

    createSubscription (notificationName notification) eu

insertEntity :: (PersistEntity e, PersistEntityBackend e ~ SqlBackend)
             => e -> Example (Entity e)
insertEntity e = do
    eid <- runDB $ insert e

    return $ Entity eid e
