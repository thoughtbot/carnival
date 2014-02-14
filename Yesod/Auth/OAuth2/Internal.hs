{-# LANGUAGE DeriveDataTypeable #-}
module Yesod.Auth.OAuth2.Internal where

{- see https://gist.github.com/qzchenwl/2351071 -}

import Prelude
import Data.Aeson
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.ByteString.Lazy (toChunks)
import Data.List
import Data.Maybe
import Data.Typeable (Typeable)
import Network.HTTP.Types (renderSimpleQuery, parseSimpleQuery)
import qualified Network.HTTP.Types as HT
import Network.HTTP.Conduit as C
import Control.Exception
import Control.Applicative ((<$>))
import Control.Monad (mzero)

data OAuth2 = OAuth2 { oauthClientId :: BS.ByteString
                     , oauthClientSecret :: BS.ByteString
                     , oauthOAuthorizeEndpoint :: BS.ByteString
                     , oauthAccessTokenEndpoint :: BS.ByteString
                     , oauthCallback :: Maybe BS.ByteString
                     , oauthAccessToken :: Maybe BS.ByteString
                     } deriving (Show, Eq)

data OAuthException = OAuthException String
                      deriving (Show, Eq, Typeable)

instance Exception OAuthException

newOAuth2 :: OAuth2
newOAuth2 = OAuth2 { oauthClientId = error "You must specify client id."
                   , oauthClientSecret = error "You must specify client secret."
                   , oauthOAuthorizeEndpoint = error "You must specify authorize endpoint."
                   , oauthAccessTokenEndpoint = error "You must specify access_token endpoint."
                   , oauthCallback = Nothing
                   , oauthAccessToken = Nothing
                   }

authorizationUrl :: OAuth2 -> BS.ByteString
authorizationUrl oa = oauthOAuthorizeEndpoint oa `BS.append` queryString
  where queryString = renderSimpleQuery True query
        query = foldr step [] [ ("client_id", Just $ oauthClientId oa)
                              , ("response_type", Just "code")
                              , ("redirect_uri", oauthCallback oa)]

request req = (withManager . httpLbs) (req { checkStatus = \_ _ _ -> Nothing })

getAccessToken' :: OAuth2 -> BS.ByteString -> Maybe BS.ByteString -> IO BSL.ByteString
getAccessToken' oa code grant_type = do
    rsp <- request req
    if (HT.statusCode . responseStatus) rsp == 200
        then return $ responseBody rsp
        else throwIO . OAuthException $ "Gaining access_token failed: " ++ BSL.unpack (responseBody rsp)
  where
    req = fromJust $ parseUrl url
    url = BS.unpack $ oauthAccessTokenEndpoint oa `BS.append` queryString
    queryString = renderSimpleQuery True query
    query = foldr step [] [ ("client_id", Just $ oauthClientId oa)
                          , ("client_secret", Just $ oauthClientSecret oa)
                          , ("code", Just code)
                          , ("redirect_uri", oauthCallback oa)
                          , ("grant_type", grant_type) ]

postAccessToken' :: OAuth2 -> BS.ByteString -> Maybe BS.ByteString -> IO BSL.ByteString
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

step :: (a, Maybe b) -> [(a, b)] -> [(a, b)]
step (a, Just b) xs = (a, b):xs
step _           xs = xs

getAccessToken :: OAuth2 -> BS.ByteString -> Maybe BS.ByteString -> IO (Maybe AccessToken)
getAccessToken oa code grant_type = decode <$> getAccessToken' oa code grant_type

postAccessToken :: OAuth2 -> BS.ByteString -> Maybe BS.ByteString -> IO (Maybe AccessToken)
postAccessToken oa code grant_type = decode <$> postAccessToken' oa code grant_type

data AccessToken = AccessToken
    { accessToken :: BS.ByteString
    } deriving (Show)

instance FromJSON AccessToken where
    parseJSON (Object o) = AccessToken <$> o .: "access_token"
    parseJSON _          = mzero

signRequest :: OAuth2 -> Request m -> Request m
signRequest oa req = req { queryString = (renderSimpleQuery False newQuery) }
  where
    newQuery = case oauthAccessToken oa of
                    Just at -> insert ("oauth_token", at) oldQuery
                    _       -> insert ("client_id", oauthClientId oa) . insert ("client_secret", oauthClientSecret oa) $ oldQuery
    oldQuery = parseSimpleQuery (queryString req)

authorizeRequest :: AccessToken -> Request m -> Request m
authorizeRequest (AccessToken token) req = req { requestHeaders = auth : requestHeaders req }
    where
        auth = ("Authorization", BS.concat ["Bearer ", token])
