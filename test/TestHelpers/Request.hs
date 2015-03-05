{-# LANGUAGE OverloadedStrings #-}
module TestHelpers.Request
    ( putBody
    , delete
    , getWithParams
    , postForm
    ) where

import Yesod.Test
import Yesod (Yesod, RedirectUrl)
import Data.Text (Text)
import Data.ByteString.Lazy (ByteString)

-- | Like @'postBody'@ but uses PUT
putBody :: (Yesod site, RedirectUrl site url)
        => url
        -> ByteString
        -> YesodExample site ()
putBody url body = request $ do
    setMethod "PUT"
    setUrl url
    setRequestBody body

-- | Perform a DELETE request
delete :: (Yesod site, RedirectUrl site url)
       => url
       -> YesodExample site ()
delete url = request $ do
    setMethod "DELETE"
    setUrl url

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
