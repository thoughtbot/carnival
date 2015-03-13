module Factories
    ( buildSite
    , createUser
    , buildComment
    , createComment
    , daysAfter
    , insertEntity
    , sometime
    ) where

import Prelude

import Model
import Settings

import Database.Persist
import Database.Persist.Sql

import Control.Applicative ((<$>))
import Data.Monoid ((<>))
import Data.Text (Text)
import Data.Time (UTCTime(..), addDays, fromGregorian, secondsToDiffTime)
import Text.Markdown (Markdown(..))

import qualified Data.Text.Lazy as TL

buildSite :: Site
buildSite = Site
    { siteName = "test-site-name"
    , siteBaseUrl = "http://localhost:4000"
    , siteLanguage = "en-us"
    }

createUser :: Text -> DB (Entity User)
createUser ident =
    insertEntity User
        { userName = "John Smith (" <> ident <> ")"
        , userEmail = "john-" <> ident <> "@gmail.com"
        , userPlugin = "dummy"
        , userIdent = ident
        }

buildComment :: Comment
buildComment = Comment
    { commentUser = someKey
    , commentSite = someKey
    , commentThread = "1"
    , commentArticleTitle = "title"
    , commentArticleURL = "http://example.com/1"
    , commentArticleAuthor = "John Smith"
    , commentBody = Markdown "hello"
    , commentCreated = sometime
    }

createComment :: UserId -> SiteId -> Text -> Text -> TL.Text -> DB (Entity Comment)
createComment uid siteId article thread body =
    insertEntity $
    buildComment
        { commentUser = uid
        , commentSite = siteId
        , commentThread = thread
        , commentArticleURL = article
        , commentBody = Markdown body
        }

insertEntity :: (PersistEntity e, PersistEntityBackend e ~ SqlBackend)
             => e -> DB (Entity e)
insertEntity e = (`Entity` e) <$> insert e

sometime :: UTCTime
sometime = UTCTime (fromGregorian 2014 2 3) (secondsToDiffTime 0)

daysAfter :: Integer -> UTCTime -> UTCTime
x `daysAfter` t = t { utctDay = addDays x (utctDay t) }

someKey :: ToBackendKey SqlBackend a => Key a
someKey = toSqlKey 1
