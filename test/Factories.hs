module Factories
    ( buildSite
    , buildPlan
    , buildUser
    , createFreePlan
    , createUser
    , createComment
    ) where

import Model
import Settings

import ClassyPrelude
import Database.Persist
import Database.Persist.Sql
import Text.Markdown (Markdown(..))

import qualified Data.Text.Lazy as TL
import qualified Web.Stripe.Plan as S

buildSite :: Site
buildSite = Site
    { siteName = "test-site-name"
    , siteBaseUrl = "http://localhost:4000"
    , siteLanguage = "en-us"
    }

buildPlan :: Plan
buildPlan = Plan
    { planName = S.PlanId "plan"
    , planDescription = "Plan"
    , planPrice = 0
    , planSiteQuota = 0
    , planCommentQuota = 0
    , planCommercial = True
    , planSort = 1
    }

buildUser :: User
buildUser = User
    { userName = "user"
    , userEmail = "user@example.com"
    , userPlugin = "dummy"
    , userIdent = "1"
    , userPlan = somePlanId
    , userStripeId = Nothing
    }

createFreePlan :: DB (Entity Plan)
createFreePlan = upsert buildPlan
    { planName = freePlanId
    , planDescription = "Personal"
    , planPrice = 0
    , planSiteQuota = 1
    , planCommentQuota = 10
    , planCommercial = False
    } []

createUser :: Text -> DB (Entity User)
createUser ident = do
    Entity planId _ <- createFreePlan

    insertEntity buildUser
        { userName = "user-" ++ ident
        , userEmail = "user-" ++ ident ++ "@example.com"
        , userIdent = ident
        , userPlan = planId
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

somePlanId :: PlanId
somePlanId = toSqlKey 1
