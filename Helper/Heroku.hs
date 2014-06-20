module Helper.Heroku (herokuConf) where

import Prelude
import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Database.Persist.Postgresql (PostgresConf(..))

import qualified Data.Text as T

-- Begin from Web.Heroku

import Network.URI
import System.Environment

dbConnParams' :: String -> (String -> [(Text, Text)]) -> IO [(Text, Text)]
dbConnParams' envVar parse = getEnv envVar >>= return . parse

parseDatabaseUrl' :: String -> String -> [(Text, Text)]
parseDatabaseUrl' scheme durl =
  let muri = parseAbsoluteURI durl
      (auth, path) = case muri of
                      Nothing ->  error "couldn't parse absolute uri"
                      Just uri -> if uriScheme uri /= scheme
                                    then schemeError uri
                                    else case uriAuthority uri of
                                           Nothing   -> invalid
                                           Just a -> (a, uriPath uri)
      (user,password) = userAndPassword auth
  in     [
          (T.pack "user",     user)
          -- tail not safe, but should be there on Heroku
         ,(T.pack "password", T.tail password)
         ,(T.pack "host",     T.pack $ uriRegName auth)
         ,(T.pack "port",     T.tail $ T.pack $ uriPort auth)
         -- tail not safe but path should always be there
         ,(T.pack "dbname",   T.pack $ Prelude.tail $ path)
         ]
  where
    -- init is not safe, but should be there on Heroku
    userAndPassword :: URIAuth -> (Text, Text)
    userAndPassword = (T.breakOn $ T.pack ":") . T.pack . Prelude.init . uriUserInfo

    schemeError uri = error $ "was expecting a postgres scheme, not: " ++ (uriScheme uri) ++ "\n" ++ (show uri)
    -- should be an error 
    invalid = error "could not parse heroku DATABASE_URL"

dbConnParams :: IO [(Text, Text)]
dbConnParams = dbConnParams' "DATABASE_URL" parseDatabaseUrl

parseDatabaseUrl :: String -> [(Text, Text)]
parseDatabaseUrl = parseDatabaseUrl' "postgres:"

-- End from Web.Heroku

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
