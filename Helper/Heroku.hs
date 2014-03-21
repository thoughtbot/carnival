module Helper.Heroku (herokuConf) where

import Prelude
import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Database.Persist.Postgresql (PostgresConf(..))
import Web.Heroku (dbConnParams)

import qualified Data.Text as T

herokuConf :: IO PostgresConf
herokuConf = do
    params <- dbConnParams

    return PostgresConf
        { pgConnStr  = formatParams params
        , pgPoolSize = 10
        }

    where
        formatParams :: [(Text, Text)] -> ByteString
        formatParams = encodeUtf8 . T.unwords . map toKeyValue

        toKeyValue :: (Text, Text) -> Text
        toKeyValue (k, v) = k `T.append` "=" `T.append` v
