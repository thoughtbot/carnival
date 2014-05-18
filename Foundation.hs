module Foundation where

import Prelude
import Yesod
import Yesod.Static
import Yesod.Auth
import Yesod.Auth.Dummy
import Yesod.Auth.OAuth2.Learn
import Yesod.Default.Config
import Yesod.Default.Util (addStaticContentExternal)
import Network.HTTP.Conduit (Manager)
import qualified Settings
import Settings.Development (development)
import qualified Database.Persist
import Database.Persist.Sql (SqlPersistT)
import Settings.StaticFiles
import Settings (widgetFile, Extra (..), LearnOAuthKeys (..))
import Model
import Text.Jasmine (minifym)
import Text.Hamlet (hamletFile)
import Yesod.Core.Types (Logger)
import Control.Applicative ((<$>), (<*>), pure)
import qualified Session as S

-- | The site argument for your application. This can be a good place to
-- keep settings and values requiring initialization before your application
-- starts running, such as database connections. Every handler will have
-- access to the data present here.
data App = App
    { settings :: AppConfig DefaultEnv Extra
    , getStatic :: Static -- ^ Settings for static file serving.
    , connPool :: Database.Persist.PersistConfigPool Settings.PersistConf -- ^ Database connection pool.
    , httpManager :: Manager
    , persistConfig :: Settings.PersistConf
    , appLogger :: Logger
    , learnOAuthKeys :: LearnOAuthKeys
    }

-- Set up i18n messages. See the message folder.
mkMessage "App" "messages" "en"

-- This is where we define all of the routes in our application. For a full
-- explanation of the syntax, please see:
-- http://www.yesodweb.com/book/routing-and-handlers
--
-- Note that this is really half the story; in Application.hs, mkYesodDispatch
-- generates the rest of the code. Please see the linked documentation for an
-- explanation for this split.
mkYesodData "App" $(parseRoutesFile "config/routes")

type Form x = Html -> MForm (HandlerT App IO) (FormResult x, Widget)

-- Please see the documentation for the Yesod typeclass. There are a number
-- of settings which can be configured by overriding methods here.
instance Yesod App where
    approot = ApprootMaster $ appRoot . settings

    makeSessionBackend _ = S.makeSessionBackend "SESSION_SECRET"

    defaultLayout widget = do
        -- We break up the default layout into two components:
        -- default-layout is the contents of the body tag, and
        -- default-layout-wrapper is the entire page. Since the final
        -- value passed to hamletToRepHtml cannot be a widget, this allows
        -- you to use normal widget features in default-layout.

        pc <- widgetToPageContent $ do
            $(combineStylesheets 'StaticR
                [ css_normalize_css
                , css_bootstrap_css
                ])
            $(widgetFile "default-layout")
        giveUrlRenderer $(hamletFile "templates/default-layout-wrapper.hamlet")

    -- This is done to provide an optimization for serving static files from
    -- a separate domain. Please see the staticRoot setting in Settings.hs
    urlRenderOverride y (StaticR s) =
        Just $ uncurry (joinPath y (Settings.staticRoot $ settings y)) $ renderRoute s
    urlRenderOverride _ _ = Nothing

    -- The page to be redirected to when authentication is required.
    authRoute _ = Just $ AuthR LoginR

    -- This function creates static content files in the static folder
    -- and names them based on a hash of their content. This allows
    -- expiration dates to be set far in the future without worry of
    -- users receiving stale content.
    addStaticContent =
        addStaticContentExternal minifym genFileName Settings.staticDir (StaticR . flip StaticRoute [])
      where
        -- Generate a unique filename based on the content itself
        genFileName lbs
            | development = "autogen-" ++ base64md5 lbs
            | otherwise   = base64md5 lbs

    -- Place Javascript at bottom of the body tag so the rest of the page loads first
    jsLoader _ = BottomOfBody

    -- What messages should be logged. The following includes all messages when
    -- in development, and warnings and errors in production.
    shouldLog _ _source level =
        development || level == LevelWarn || level == LevelError

    makeLogger = return . appLogger

embedLayout :: Widget -> Handler Html
embedLayout widget = do
    -- We break up the default layout into two components:
    -- default-layout is the contents of the body tag, and
    -- default-layout-wrapper is the entire page. Since the final
    -- value passed to hamletToRepHtml cannot be a widget, this allows
    -- you to use normal widget features in default-layout.

    pc <- widgetToPageContent $ do
        widget
    giveUrlRenderer $(hamletFile "templates/embed-layout-wrapper.hamlet")

-- How to run database actions.
instance YesodPersist App where
    type YesodPersistBackend App = SqlPersistT
    runDB = defaultRunDB persistConfig connPool
instance YesodPersistRunner App where
    getDBRunner = defaultGetDBRunner connPool

instance YesodAuth App where
    type AuthId App = UserId

    -- Where to send a user after successful login
    loginDest _ = SessionR

    -- Where to send a user after logout
    logoutDest _ = SessionR

    getAuthId creds = runDB $ do
        muser <- getBy $ UniqueUser $ credsIdent creds

        let newUser = buildUser creds

        case muser of
            Just (Entity uid _) -> fmap Just $ replaceUser uid newUser
            _                   -> insertUser newUser

    authPlugins m = addAuthBackDoor
        [ oauth2Learn
            (learnOauthClientId $ learnOAuthKeys m)
            (learnOauthClientSecret $ learnOAuthKeys m)
        ]

    authHttpManager = httpManager

buildUser :: Creds m -> Maybe User
buildUser (Creds _ csId csExtra) =
    User <$> lookup "first_name" csExtra
         <*> lookup "last_name" csExtra
         <*> lookup "email" csExtra
         <*> pure csId

replaceUser :: UserId -> Maybe User -> YesodDB App UserId
replaceUser uid (Just u) = replace uid u >> return uid
replaceUser uid _        = return uid

insertUser :: Maybe User -> YesodDB App (Maybe UserId)
insertUser (Just u) = fmap Just $ insert u
insertUser _        = return Nothing

addAuthBackDoor :: [AuthPlugin App] -> [AuthPlugin App]
addAuthBackDoor = if development then (authDummy :) else id

-- This instance is required to use forms. You can modify renderMessage to
-- achieve customized and internationalized form validation messages.
instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage

-- | Get the 'Extra' value, used to hold data from the settings.yml file.
getExtra :: Handler Extra
getExtra = fmap (appExtra . settings) getYesod

-- Note: previous versions of the scaffolding included a deliver function to
-- send emails. Unfortunately, there are too many different options for us to
-- give a reasonable default. Instead, the information is available on the
-- wiki:
--
-- https://github.com/yesodweb/yesod/wiki/Sending-email
