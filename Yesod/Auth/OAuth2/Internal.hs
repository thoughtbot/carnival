{-# OPTIONS_GHC -fno-warn-orphans #-}
module Yesod.Auth.OAuth2.Internal
    ( OAuth2(..)
    , newOAuth2
    , authorizationUrl
    , AccessToken(..)
    , postAccessToken
    ) where

import Prelude

import Control.Applicative ((<$>), pure)
import Control.Exception
import Control.Monad (mzero)
import Data.Aeson
import Data.Maybe
import Data.Text.Encoding (encodeUtf8)
import Data.Typeable (Typeable)
import Network.HTTP.Conduit as C
import Network.HTTP.Types as HT

import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL

data OAuth2 = OAuth2
    { oauthClientId            :: BS.ByteString
    , oauthClientSecret        :: BS.ByteString
    , oauthOAuthorizeEndpoint  :: BS.ByteString
    , oauthAccessTokenEndpoint :: BS.ByteString
    , oauthCallback            :: Maybe BS.ByteString
    , oauthAccessToken         :: Maybe BS.ByteString
    } deriving (Show, Eq)

data OAuthException = OAuthException String deriving (Show, Eq, Typeable)

instance Exception OAuthException

newOAuth2 :: OAuth2
newOAuth2 = OAuth2
    { oauthClientId            = error "You must specify client id."
    , oauthClientSecret        = error "You must specify client secret."
    , oauthOAuthorizeEndpoint  = error "You must specify authorize endpoint."
    , oauthAccessTokenEndpoint = error "You must specify access_token endpoint."
    , oauthCallback            = Nothing
    , oauthAccessToken         = Nothing
    }

authorizationUrl :: OAuth2 -> BS.ByteString
authorizationUrl oa = oauthOAuthorizeEndpoint oa `BS.append` qs

  where
    qs :: BS.ByteString
    qs = renderSimpleQuery True
       $ foldr step [] [ ("client_id"    , Just $ oauthClientId oa)
                       , ("response_type", Just "code")
                       , ("redirect_uri" , oauthCallback oa)
                       ]

data AccessToken = AccessToken
    { accessToken :: BS.ByteString
    } deriving (Show)

instance FromJSON BS.ByteString where
    parseJSON (String t) = pure $ encodeUtf8 t
    parseJSON _          = mzero

instance FromJSON AccessToken where
    parseJSON (Object o) = AccessToken <$> o .: "access_token"
    parseJSON _          = mzero

postAccessToken :: OAuth2 -> BS.ByteString -> Maybe BS.ByteString -> IO (Maybe AccessToken)
postAccessToken oa code grant_type = decode <$> postAccessToken' oa code grant_type

postAccessToken' :: OAuth2
                 -> BS.ByteString
                 -> Maybe BS.ByteString
                 -> IO BSL.ByteString
postAccessToken' oa code grant_type = do
    rsp <- request req
    if (HT.statusCode . responseStatus) rsp == 200
        then return $ responseBody rsp
        else throwIO . OAuthException $ "Gaining access_token failed: " ++ BSL.unpack (responseBody rsp)
  where
    toPost r = r { method = "POST" }
    req = urlEncodedBody query . toPost . fromJust $ parseUrl url
    url = BS.unpack $ oauthAccessTokenEndpoint oa
    query = foldr step [] [ ("client_id", Just $ oauthClientId oa)
                          , ("client_secret", Just $ oauthClientSecret oa)
                          , ("code", Just code)
                          , ("redirect_uri", oauthCallback oa)
                          , ("grant_type", grant_type) ]

request :: Request -> IO (Response BSL.ByteString)
request req =
    (withManager . httpLbs)
    (req { checkStatus = \_ _ _ -> Nothing })

step :: (a, Maybe b) -> [(a, b)] -> [(a, b)]
step (a, Just b) xs = (a, b):xs
step _           xs = xs
