{-# OPTIONS_GHC -fno-warn-type-defaults #-}
-- |
--
-- This module provides a session backend which behaves like the default session
-- backend, but does not use the file system for storing the session key.
--
-- Rather, if the named environment variable is present, its value will be used
-- to generate the session key. By setting the same value in multiple processes,
-- the user will experience a consistent session even in the presence of load
-- balancing.
--
module Session (makeSessionBackend) where

import Prelude

import Yesod hiding (makeSessionBackend)
import Web.ClientSession (getKeyEnv)

makeSessionBackend :: String -> IO (Maybe SessionBackend)
makeSessionBackend name = fmap Just $ do
    let minutes = (120 * 60) -- 120 minutes
        timeout = fromIntegral (minutes * 60)

    key <- getKeyEnv name

    (getCachedDate, _) <- clientSessionDateCacher timeout

    return SessionBackend
        { sbLoadSession = loadClientSession key getCachedDate "_SESSION"
        }
