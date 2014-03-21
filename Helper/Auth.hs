module Helper.Auth
    ( requireAuth_
    , requireAuthId_
    , module Yesod.Auth
    ) where

import Import
import Yesod.Auth
import Data.Maybe (fromMaybe)
import Control.Monad (liftM2)

-- | Like @'requireAuth'@ except that it may respond
--   @'notAuthenticated'@ instead of redirecting to login.
requireAuth_ :: Handler (Entity User)
requireAuth_ = liftM2 fromMaybe notAuthenticated maybeAuth

-- | Like @'requireAuthId'@ except that it may respond
--   @'notAuthenticated'@ instead of redirecting to login.
requireAuthId_ :: Handler UserId
requireAuthId_ = liftM2 fromMaybe notAuthenticated maybeAuthId
