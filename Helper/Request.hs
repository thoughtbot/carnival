module Helper.Request (allowCrossOrigin) where

import Import
import Network.Wai (requestHeaders)
import Network.HTTP.Types (RequestHeaders)
import Data.Text.Encoding (decodeUtf8)

remoteOrigin :: Handler (Maybe Text)
remoteOrigin = return . getOrigin . requestHeaders =<< waiRequest

getOrigin :: RequestHeaders -> Maybe Text
getOrigin = (fmap decodeUtf8) . lookup "Origin"

allowCrossOrigin :: Handler ()
allowCrossOrigin = do
    mo <- remoteOrigin

    case mo of
        Just o  -> addHeader "Access-Control-Allow-Origin" o
        Nothing -> return ()

    addHeader "Access-Control-Allow-Methods" "*"
    addHeader "Access-Control-Allow-Credentials" "true"
