-- | Settings are centralized, as much as possible, into this file. This
-- includes database connection settings, static file locations, etc.
-- In addition, you can configure a number of different aspects of Yesod
-- by overriding methods in the Yesod typeclass. That instance is
-- declared in the Foundation.hs file.
module Settings where

import Prelude
import Text.Shakespeare.Text (st)
import Language.Haskell.TH.Syntax
import Database.Persist.Postgresql (PostgresConf)
import Yesod.Default.Config
import Yesod.Default.Util
import Data.Text (Text)
import Data.Yaml
import Control.Applicative
import Settings.Development
import Data.Default (def)
import Text.Hamlet
import Text.Coffee
import Text.Lucius

-- | Which Persistent backend this site is using.
type PersistConf = PostgresConf

-- Static setting below. Changing these requires a recompile

-- | The location of static files on your system. This is a file system
-- path. The default value works properly with your scaffolded site.
staticDir :: FilePath
staticDir = "static"

-- | The base URL for your static files. As you can see by the default
-- value, this can simply be "static" appended to your application root.
-- A powerful optimization can be serving static files from a separate
-- domain name. This allows you to use a web server optimized for static
-- files, more easily set expires and cache values, and avoid possibly
-- costly transference of cookies on static files. For more information,
-- please see:
--   http://code.google.com/speed/page-speed/docs/request.html#ServeFromCookielessDomain
--
-- If you change the resource pattern for StaticR in Foundation.hs, you will
-- have to make a corresponding change here.
--
-- To see how this value is used, see urlRenderOverride in Foundation.hs
staticRoot :: AppConfig DefaultEnv x -> Text
staticRoot conf = [st|#{appRoot conf}/static|]

-- | Settings for 'widgetFile', such as which template languages to support and
-- default Hamlet settings.
--
-- For more information on modifying behavior, see:
--
-- https://github.com/yesodweb/yesod/wiki/Overriding-widgetFile
widgetFileSettings :: WidgetFileSettings
widgetFileSettings = def
    { wfsLanguages = \hset -> defaultTemplateLanguages hset ++
        [ TemplateLanguage True  "coffee"  Text.Coffee.coffeeFile   Text.Coffee.coffeeFileReload
        ]
    , wfsHamletSettings = defaultHamletSettings
        { hamletNewlines = AlwaysNewlines
        }
    }

-- The rest of this file contains settings which rarely need changing by a
-- user.

widgetFile :: String -> Q Exp
widgetFile = (if development then widgetFileReload
                             else widgetFileNoReload)
              widgetFileSettings

coffeeFile :: String -> Q Exp
coffeeFile f = (if development then Text.Coffee.coffeeFileReload $ globFile "coffee" f
                               else Text.Coffee.coffeeFile $ globFile "coffee" f)

luciusFile :: String -> Q Exp
luciusFile f = (if development then Text.Lucius.luciusFileReload $ globFile "lucius" f
                               else Text.Lucius.luciusFile $ globFile "lucius" f)

data Extra = Extra
    { extraCopyright :: Text
    , extraAnalytics :: Maybe Text -- ^ Google Analytics
    } deriving Show

parseExtra :: DefaultEnv -> Object -> Parser Extra
parseExtra _ o = Extra
    <$> o .:  "copyright"
    <*> o .:? "analytics"

data LearnOAuthKeys = LearnOAuthKeys
    { learnOauthClientId     :: Text
    , learnOauthClientSecret :: Text
    }

learnOAuthKeysDev :: LearnOAuthKeys
learnOAuthKeysDev = LearnOAuthKeys
    { learnOauthClientId     = "aa02cb577894ae12346b2cf7804514fefd4735d40896d51638f341da0782ba9a"
    , learnOauthClientSecret = "74061353de336c0befd9ef20dc2902ddc80c6101e30f4e913cb2f258566ea8a0"
    }
