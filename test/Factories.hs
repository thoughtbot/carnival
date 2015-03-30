module Factories
    ( buildSite
    , createUser
    , createComment
    ) where

import Model
import Settings

import Database.Persist
import Database.Persist.Sql

import Control.Applicative ((<$>))
import Control.Monad.IO.Class (liftIO)
import Data.Monoid ((<>))
import Data.Text (Text)
import Data.Time (getCurrentTime)
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

createComment :: UserId -> SiteId -> Text -> Text -> TL.Text -> DB (Entity Comment)
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
