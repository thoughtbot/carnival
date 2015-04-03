module Factories
    ( buildSite
    , createFreePlan
    , createUser
    , createComment
    ) where

import Model
import Model.User
import Settings

import ClassyPrelude
import Database.Persist
import Text.Markdown (Markdown(..))

import qualified Data.Text.Lazy as TL

buildSite :: Site
buildSite = Site
    { siteName = "test-site-name"
    , siteBaseUrl = "http://localhost:4000"
    , siteLanguage = "en-us"
    }

createFreePlan :: DB (Entity Plan)
createFreePlan = upsert Plan
    { planName = freePlanId
    , planDescription = "Personal"
    , planPrice = 0
    , planSiteQuota = 1
    , planCommentQuota = 10
    , planBranded = True
    , planSort = 1
    } []

createUser :: Text -> DB (Entity User)
createUser ident = do
    Entity planId _ <- createFreePlan

    insertEntity User
        { userName = profileName dummyProfile
        , userEmail = profileEmail dummyProfile
        , userPlugin = "dummy"
        , userIdent = ident
        , userPlan = planId
        , userStripeId = Nothing
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
