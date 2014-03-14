module Helper.Auth
    ( requireAuth_
    , requireAuthId_
    , module Yesod.Auth
    ) where

import Import
import Yesod.Auth

-- | Like @'requireAuth'@ except that it may respond
--   @'notAuthenticated'@ instead of redirecting to login.
requireAuth_ :: Handler (Entity User)
requireAuth_ = maybeAuth `whenNothing` notAuthenticated

-- | Like @'requireAuthId'@ except that it may respond
--   @'notAuthenticated'@ instead of redirecting to login.
requireAuthId_ :: Handler UserId
requireAuthId_ = maybeAuthId `whenNothing` notAuthenticated

whenNothing :: Monad m => m (Maybe a) -> m a -> m a
whenNothing ma b = do
    mv <- ma

    case mv of
        Just v  -> return v
        Nothing -> b
