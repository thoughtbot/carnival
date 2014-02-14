{-# LANGUAGE DeriveDataTypeable, OverloadedStrings, QuasiQuotes #-}
module Yesod.Auth.OAuth2 where

import Control.Monad.IO.Class
import Data.ByteString          (ByteString)
import Data.Text                (Text)
import Data.Text.Encoding       (decodeUtf8With, encodeUtf8)
import Data.Text.Encoding.Error (lenientDecode)
import Data.Maybe
import Network.HTTP.Conduit as C
import Yesod.Auth
import Yesod.Form
import Yesod.Core
import Yesod.Auth.OAuth2.Internal

oauth2Url :: Text -> AuthRoute
oauth2Url name = PluginR name ["forward"]

authOAuth2 name oauth getCreds = AuthPlugin name dispatch login
    where
        url = PluginR name ["callback"]
        dispatch "GET" ["forward"] = do
            tm <- getRouteToParent
            lift $ do
                render <- getUrlRender
                let oauth' = oauth { oauthCallback = Just $ encodeUtf8 $ render $ tm url }
                redirect $ authorizationUrl oauth'
        dispatch "GET" ["callback"] = do
            code <- lift $ runInputGet $ ireq textField "code"
            mtoken <- liftIO $ postAccessToken oauth (encodeUtf8 code) (Just "authorization_code")
            case mtoken of
                Nothing -> permissionDenied "Couldn't get token"
                Just token -> getCreds token
        disptach _ _ = notFound
        login tm = do
            render <- getUrlRender
            let oaUrl = render $ tm $ oauth2Url name
            [whamlet| <a href=#{oaUrl}>Login via #{name} |]

oauth2Google clientId clientSecret = newOAuth2 { oauthClientId = encodeUtf8 clientId
                                               , oauthClientSecret = encodeUtf8 clientSecret
                                               , oauthOAuthorizeEndpoint = "https://accounts.google.com/o/oauth2/auth"
                                               , oauthAccessTokenEndpoint = "https://accounts.google.com/o/oauth2/token" }

cloudsdaleAuth clientId clientSecret = authOAuth2 "cloudsdale" oauth2 $ \token -> do
    rsp <- request $ authorizeRequest token $ fromJust $ parseUrl "http://api.cloudsdale.org/v2/me.json"
    undefined
    where
        oauth2 = newOAuth2 { oauthClientId = encodeUtf8 clientId
                           , oauthClientSecret = encodeUtf8 clientSecret
                           , oauthOAuthorizeEndpoint = "http://www.cloudsdale.org/oauth/authorize"
                           , oauthAccessTokenEndpoint = "http://www.cloudsdale.org/oauth/token" }

authLearn key secret = newOAuth2
    { oauthClientId = encodeUtf8 key
    , oauthClientSecret = encodeUtf8 secret
    , oauthOAuthorizeEndpoint = "http://learn.thoughtbot.com/oauth/authorize"
    , oauthAccessTokenEndpoint = "http://learn.thoughtbot.com/oauth/token"
    }

bsToText :: ByteString -> Text
bsToText = decodeUtf8With lenientDecode
