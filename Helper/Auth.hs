module Helper.Auth
    ( requireAuth_
    , requireMemberSite
    ) where

import Import
import Model.Site
import Helper.Request

-- | Like @'requireAuth'@ except that it may respond
--   @'notAuthenticated'@ instead of redirecting to login.
requireAuth_ :: Handler (Entity User)
requireAuth_ = fromMaybeM notAuthenticated maybeAuth

requireMemberSite :: SiteId -> Handler Site
requireMemberSite siteId = do
    userId <- requireAuthId
    fromMaybe404 $ runDB $ findMemberSite siteId userId

-- N.B. This should be equivalent to liftM fromMaybe, but that form is
-- evaluating the first argument regardless of the second's Just-ness,
-- resulting in constant notAuthenticated results
fromMaybeM :: Monad m => m a -> m (Maybe a) -> m a
fromMaybeM ma mmb = do
    mb <- mmb

    case mb of
        Nothing -> ma
        Just b  -> return b
