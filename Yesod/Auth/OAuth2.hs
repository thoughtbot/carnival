module Yesod.Auth.OAuth2
    ( authOAuth2
    , authLearn
    , oauth2Google
    ) where

import Prelude
import Control.Monad.IO.Class
import Data.ByteString.Internal (ByteString)
import Data.Text                (Text)
import Data.Text.Encoding       (encodeUtf8, decodeUtf8With)
import Data.Text.Encoding.Error (lenientDecode)
import Yesod.Auth
import Yesod.Core
import Yesod.Form

import Yesod.Auth.OAuth2.Internal

oauth2Url :: Text -> AuthRoute
oauth2Url name = PluginR name ["forward"]

authOAuth2 :: YesodAuth m
           => Text -- ^ Service name
           -> OAuth2
           -> AuthPlugin m
authOAuth2 name oauth = AuthPlugin name dispatch login

    where
        url = PluginR name ["callback"]

        dispatch "GET" ["forward"] = do
            tm <- getRouteToParent
            lift $ do
                render <- getUrlRender
                let oauth' = oauth { oauthCallback = Just $ encodeUtf8 $ render $ tm url }
                redirect $ bsToText $ authorizationUrl oauth'

        dispatch "GET" ["callback"] = do
            code <- lift $ runInputGet $ ireq textField "code"
            mtoken <- liftIO $ postAccessToken oauth (encodeUtf8 code) (Just "authorization_code")
            case mtoken of
                Nothing    -> permissionDenied "Couldn't get token"
                Just token -> getCreds token

        dispatch _ _ = notFound

        login tm = do
            render <- getUrlRender
            let oaUrl = render $ tm $ oauth2Url name
            [whamlet| <a href=#{oaUrl}>Login via #{name} |]

-- TODO Parse identifier out of OAuth2 response
getCreds = undefined

authLearn :: Text -> Text -> OAuth2
authLearn clientId clientSecret = newOAuth2
    { oauthClientId            = encodeUtf8 clientId
    , oauthClientSecret        = encodeUtf8 clientSecret
    , oauthOAuthorizeEndpoint  = "http://learn.thoughtbot.com/oauth/authorize"
    , oauthAccessTokenEndpoint = "http://learn.thoughtbot.com/oauth/token"
    }

oauth2Google :: Text -> Text -> OAuth2
oauth2Google clientId clientSecret = newOAuth2
    { oauthClientId            = encodeUtf8 clientId
    , oauthClientSecret        = encodeUtf8 clientSecret
    , oauthOAuthorizeEndpoint  = "https://accounts.google.com/o/oauth2/auth"
    , oauthAccessTokenEndpoint = "https://accounts.google.com/o/oauth2/token"
    }

bsToText :: ByteString -> Text
bsToText = decodeUtf8With lenientDecode
