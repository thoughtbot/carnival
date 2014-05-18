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
-- Notes:
--
-- 1. If a value is not present or is invalid, a random key will be used which
--    only lives as long as that process.
--
-- 2. The value must be exactly 96 bytes. An appropriate string can be generated
--    on most unix-like commandlines in the following way:
--
-- > hexdump -v -n "48" -e '1/1 "%02x"' /dev/urandom
--
module Session (makeSessionBackend) where

import Prelude

import System.Environment (lookupEnv)
import qualified Data.ByteString.Char8 as BS

import Yesod hiding (makeSessionBackend)
import qualified Web.ClientSession as CS

makeSessionBackend :: String -> IO (Maybe SessionBackend)
makeSessionBackend name = fmap Just $ do
    let minutes = (120 * 60) -- 120 minutes
        timeout = fromIntegral (minutes * 60)

    key <- getKey name

    (getCachedDate, _) <- clientSessionDateCacher timeout

    return SessionBackend
        { sbLoadSession = loadClientSession key getCachedDate "_SESSION"
        }

getKey :: String -> IO CS.Key
getKey name = do
    mval <- lookupEnv name

    maybe newKey return $ initKey =<< mval

  where
    newKey :: IO CS.Key
    newKey = fmap snd $ CS.randomKey

    initKey :: String -> Maybe CS.Key
    initKey = eitherToMaybe . CS.initKey . BS.pack

    eitherToMaybe :: Either a b -> Maybe b
    eitherToMaybe = either (const Nothing) Just
