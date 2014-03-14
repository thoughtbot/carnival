module Helper.Auth
    ( requireAuth_
    , module Yesod.Auth
    ) where

import Import
import Yesod.Auth

-- | Like @'requireAuth'@ except that it may respond
--   @'notAuthenticated'@ instead of redirecting to login.
requireAuth_ :: Handler (Entity User)
requireAuth_ = do
    muser <- maybeAuth

    case muser of
        Just user -> return user
        Nothing   -> notAuthenticated
