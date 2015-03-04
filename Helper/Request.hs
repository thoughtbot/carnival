module Helper.Request
    ( allowCrossOrigin
    , fromMaybe404
    ) where

import Import
import Network.HTTP.Types (HeaderName)
import Data.Text.Encoding (decodeUtf8)

lookupUtf8Header :: HeaderName -> Handler (Maybe Text)
lookupUtf8Header headerName = return . fmap decodeUtf8 =<< lookupHeader headerName

allowCrossOrigin :: Handler ()
allowCrossOrigin = do
    mo <- lookupUtf8Header "Origin"
    mrh <- lookupUtf8Header "Access-Control-Request-Headers"

    case mo of
        Just o  -> addHeader "Access-Control-Allow-Origin" o
        Nothing -> return ()

    case mrh of
        Just rh -> addHeader "Access-Control-Allow-Headers" rh
        Nothing -> return ()

    addHeader "Access-Control-Allow-Methods" "POST, GET, OPTIONS"
    addHeader "Access-Control-Allow-Credentials" "true"

fromMaybe404 :: Handler (Maybe a) -> Handler a
fromMaybe404 f = maybe notFound return =<< f
