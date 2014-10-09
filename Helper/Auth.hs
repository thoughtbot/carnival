module Helper.Auth
    ( requireAuth_
    , requireAuthId_
    , requireOwnComment
    , module Yesod.Auth
    ) where

import Import
import Yesod.Auth

import Control.Monad (when)

-- | Like @'requireAuth'@ except that it may respond
--   @'notAuthenticated'@ instead of redirecting to login.
requireAuth_ :: Handler (Entity User)
requireAuth_ = fromMaybeM notAuthenticated maybeAuth

-- | Like @'requireAuthId'@ except that it may respond
--   @'notAuthenticated'@ instead of redirecting to login.
requireAuthId_ :: Handler UserId
requireAuthId_ = fromMaybeM notAuthenticated maybeAuthId

requireOwnComment :: Comment -> UserId -> Handler ()
requireOwnComment comment userId = do
    let commentUserId = commentUser comment

    when (userId /= commentUserId) $
        permissionDenied "Action only appropriate for your own comments"

-- N.B. This should be equivalent to liftM fromMaybe, but that form is
-- evaluating the first argument regardless of the second's Just-ness,
-- resulting in constant notAuthenticated results
fromMaybeM :: Monad m => m a -> m (Maybe a) -> m a
fromMaybeM ma mmb = do
    mb <- mmb

    case mb of
        Nothing -> ma
        Just b  -> return b
