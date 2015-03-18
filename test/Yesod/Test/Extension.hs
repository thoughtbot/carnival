module Yesod.Test.Extension
    ( getWithParams
    , postForJSON
    , postForm
    , valueEquals
    , module Yesod.Test
    ) where

import Prelude

import Yesod.Test
import Test.Hspec.Expectations.Lifted
import Yesod (Yesod, RedirectUrl)
import Data.Aeson (Value, eitherDecode)
import Data.Text (Text)
import Network.HTTP.Types (hAccept)
import Network.Wai.Test (simpleBody)

-- | Perform a GET request with query params present
getWithParams :: (RedirectUrl site url, Yesod site)
              => url
              -> [(Text, Text)]
              -> YesodExample site ()
getWithParams url params = request $ do
    setMethod  "GET"
    mapM_ (uncurry addGetParam) params
    setUrl url

-- | Perform a POST request with an Accept header of "application/json"
postForJSON :: (Yesod site, RedirectUrl site url)
            => url
            -> YesodExample site ()
postForJSON url = request $ do
    addRequestHeader (hAccept, "application/json")
    setMethod "POST"
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

-- | Like @bodyEquals@ but taking a @'Value'@ argument and comparing it against
--   the response body decoded as JSON.
--
--   > valueEquals $ object ["comments" .= comments]
--
valueEquals :: Value -> YesodExample site ()
valueEquals v = withResponse $
    either (const failure) (v `shouldBe`) . eitherDecode . simpleBody

  where
    failure :: YesodExample site ()
    failure = expectationFailure "expected JSON response"
