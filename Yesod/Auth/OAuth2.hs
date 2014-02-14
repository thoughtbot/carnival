module Yesod.Auth.OAuth2 where
    -- ( authOAuth2
    -- , authLearn
    -- , oauth2Google
    -- ) where

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

import Control.Applicative ((<$>), (<*>))
import Control.Monad (mzero)

import qualified Data.Text as T

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
            tm <- getRouteToParent
            render <- lift $ getUrlRender
            code <- lift $ runInputGet $ ireq textField "code"
            let oauth' = oauth { oauthCallback = Just $ encodeUtf8 $ render $ tm url }
            mtoken <- liftIO $ postAccessToken oauth' (encodeUtf8 code) (Just "authorization_code")
            case mtoken of
                Nothing    -> permissionDenied "Couldn't get token"
                Just token -> do
                    creds <- liftIO $ mkCreds token
                    lift $ setCreds True creds

        dispatch _ _ = notFound

        login tm = do
            render <- getUrlRender
            let oaUrl = render $ tm $ oauth2Url name
            [whamlet| <a href=#{oaUrl}>Login via #{name} |]

data ProfileResponse = ProfileResponse
    { prId :: Int
    , prFirstName :: Text
    , prLastName :: Text
    , prEmail :: Text
    } deriving Show

instance FromJSON ProfileResponse where
    parseJSON (Object o) = ProfileResponse
                         <$> o .: "id"
                         <*> o .: "first_name"
                         <*> o .: "last_name"
                         <*> o .: "email"

    parseJSON _          = mzero

data NestedProfileResponse = NestedProfileResponse
    { nprUser :: ProfileResponse
    } deriving Show

instance FromJSON NestedProfileResponse where
    parseJSON (Object o) = NestedProfileResponse <$> o .: "user"
    parseJSON _          = mzero

mkCreds token = do
    Just resp <- getWithToken token "http://learn.thoughtbot.com/api/v1/me.json"

    let pResp = nprUser resp

    -- use access token to request profile info
    return $ Creds "learn" (T.pack $ show $ prId pResp)
           [ ("first_name", prFirstName pResp)
           , ("last_name" , prLastName pResp)
           , ("email"     , prEmail pResp)
           ]

-- -- TODO Parse identifier out of OAuth2 response
-- --getCreds :: AccessToken -> IO (Creds m)
-- getCreds :: AccessToken -> Creds m
-- getCreds (AccessToken bs) = Creds "carnival" (bsToText bs) []

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
