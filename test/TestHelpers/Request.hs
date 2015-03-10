{-# LANGUAGE OverloadedStrings #-}
module TestHelpers.Request
    ( getWithParams
    , postForm
    ) where

import Yesod.Test
import Yesod (Yesod, RedirectUrl)
import Data.Text (Text)

-- | Perform a GET request with query params present
getWithParams :: (RedirectUrl site url, Yesod site)
              => url
              -> [(Text, Text)]
              -> YesodExample site ()
getWithParams url params = request $ do
    setMethod  "GET"
    mapM_ (uncurry addGetParam) params
    setUrl url

postForm :: (Yesod site, RedirectUrl site url)
         => url
         -> RequestBuilder site ()
         -> YesodExample site ()
postForm url fill = do
    get url -- need the CSRF token

    request $ do
        addToken

        fill

        setMethod "POST"
        setUrl url
