module Foundation where

import Import.NoFoundation
import Database.Persist.Sql (ConnectionPool, runSqlPool)
import Text.Hamlet          (hamletFile)
import Text.Jasmine         (minifym)
import Yesod.Default.Util   (addStaticContentExternal)
import Yesod.Core.Types     (Logger)
import qualified Yesod.Core.Unsafe as Unsafe

import Model.User
import Yesod.Auth.Dummy
import Yesod.Auth.GoogleEmail2 hiding (Token)
import Yesod.Auth.OAuth2.Github

-- | The foundation datatype for your application. This can be a good place to
-- keep settings and values requiring initialization before your application
-- starts running, such as database connections. Every handler will have
-- access to the data present here.
data App = App
    { appSettings    :: AppSettings
    , appStatic      :: Static -- ^ Settings for static file serving.
    , appConnPool    :: ConnectionPool -- ^ Database connection pool.
    , appHttpManager :: Manager
    , appLogger      :: Logger
    , appGithubOAuthKeys :: OAuthKeys
    , appGoogleOAuthKeys :: OAuthKeys
    , appStripeKeys  :: StripeKeys
    }

instance HasHttpManager App where
    getHttpManager = appHttpManager

-- This is where we define all of the routes in our application. For a full
-- explanation of the syntax, please see:
-- http://www.yesodweb.com/book/routing-and-handlers
--
-- Note that this is really half the story; in Application.hs, mkYesodDispatch
-- generates the rest of the code. Please see the linked documentation for an
-- explanation for this split.
mkYesodData "App" $(parseRoutesFile "config/routes")

-- | A convenient synonym for creating forms.
type Form x = Html -> MForm (HandlerT App IO) (FormResult x, Widget)

-- Please see the documentation for the Yesod typeclass. There are a number
-- of settings which can be configured by overriding methods here.
instance Yesod App where
    -- Controls the base of generated URLs. For more information on modifying,
    -- see: https://github.com/yesodweb/yesod/wiki/Overriding-approot
    approot = ApprootMaster $ appRoot . appSettings

    -- Store session data on the client in encrypted cookies,
    -- default session idle timeout is 120 minutes
    makeSessionBackend _ = whenSSL sslOnlySessions $
        Just <$> envClientSessionBackend sessionTimeout "SESSION_KEY"

    defaultLayout widget = do
        master <- getYesod
        mmsg <- getMessage
        muser <- maybeAuth
        route <- getCurrentRoute

        let isRoute r = route == Just r

        -- We break up the default layout into two components:
        -- default-layout is the contents of the body tag, and
        -- default-layout-wrapper is the entire page. Since the final
        -- value passed to hamletToRepHtml cannot be a widget, this allows
        -- you to use normal widget features in default-layout.

        pc <- widgetToPageContent $ do
            addScript $ StaticR js_highlight_js
            addStylesheet $ StaticR css_normalize_css
            addStylesheet $ StaticR css_highlight_css

            when (maybe False includeBootstrap route) $
                addStylesheet $ StaticR css_bootstrap_css

            addStylesheet $ StaticR css_screen_css
            addStylesheetRemote $ "//fonts.googleapis.com/css?family=Lato:400,900"
            $(widgetFile "default-layout")
        withUrlRenderer $(hamletFile "templates/default-layout-wrapper.hamlet")

    -- The page to be redirected to when authentication is required.
    authRoute _ = Just $ AuthR LoginR

    -- Routes not requiring authentication.
    -- isAuthorized (AuthR _) _ = return Authorized
    -- isAuthorized FaviconR _ = return Authorized
    -- isAuthorized RobotsR _ = return Authorized
    -- -- Default to Authorized for now.
    -- isAuthorized _ _ = return Authorized

    -- This function creates static content files in the static folder
    -- and names them based on a hash of their content. This allows
    -- expiration dates to be set far in the future without worry of
    -- users receiving stale content.
    addStaticContent ext mime content = do
        master <- getYesod
        let staticDir = appStaticDir $ appSettings master
        addStaticContentExternal
            minifym
            genFileName
            staticDir
            (StaticR . flip StaticRoute [])
            ext
            mime
            content
      where
        -- Generate a unique filename based on the content itself
        genFileName lbs = "autogen-" ++ base64md5 lbs

    -- What messages should be logged. The following includes all messages when
    -- in development, and warnings and errors in production.
    shouldLog app _source level =
        appShouldLogAll (appSettings app)
            || level == LevelWarn
            || level == LevelError

    makeLogger = return . appLogger

    yesodMiddleware =
        whenSSL (sslOnlyMiddleware sessionTimeout) . defaultYesodMiddleware

whenSSL :: (a -> a) -> (a -> a)
whenSSL f = if (appForceSSL compileTimeAppSettings) then f else id

sessionTimeout :: Int
sessionTimeout = 120

embedLayout :: Widget -> Handler Html
embedLayout widget = do
    pc <- widgetToPageContent widget
    withUrlRenderer $(hamletFile "templates/embed-layout-wrapper.hamlet")

analytics :: App -> Maybe (Entity User) -> a -> Html
analytics app muser = $(hamletFile "templates/analytics.hamlet")

includeBootstrap :: Route App -> Bool
includeBootstrap RootR = False
includeBootstrap _ = True

-- How to run database actions.
instance YesodPersist App where
    type YesodPersistBackend App = SqlBackend
    runDB action = do
        master <- getYesod
        runSqlPool action $ appConnPool master
instance YesodPersistRunner App where
    getDBRunner = defaultGetDBRunner appConnPool

instance YesodAuth App where
    type AuthId App = UserId

    -- Where to send a user after successful login
    loginDest _ = RootR
    -- Where to send a user after logout
    logoutDest _ = RootR
    -- Override the above two destinations when a Referer: header is present
    redirectToReferer _ = True

    authenticate = runDB . authenticateUser

    -- You can add other plugins like BrowserID, email or OAuth here
    authPlugins m = addAuthBackDoor m
        [ oauth2Github
            (oauthKeysClientId $ appGithubOAuthKeys m)
            (oauthKeysClientSecret $ appGithubOAuthKeys m)
        , authGoogleEmail
            (oauthKeysClientId $ appGoogleOAuthKeys m)
            (oauthKeysClientSecret $ appGoogleOAuthKeys m)
        ]

    authHttpManager = getHttpManager

    loginHandler = lift $ do
        murl <- runInputGet $ iopt textField "dest"
        mapM_ setUltDest murl

        defaultLayout $ do
            setTitle "Carnival - Login"
            $(widgetFile "login")

addAuthBackDoor :: App -> [AuthPlugin App] -> [AuthPlugin App]
addAuthBackDoor app =
    if appAllowDummyAuth (appSettings app) then (authDummy :) else id

instance YesodAuthPersist App

-- This instance is required to use forms. You can modify renderMessage to
-- achieve customized and internationalized form validation messages.
instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage

unsafeHandler :: App -> Handler a -> IO a
unsafeHandler = Unsafe.fakeHandlerGetLogger appLogger

-- Note: Some functionality previously present in the scaffolding has been
-- moved to documentation in the Wiki. Following are some hopefully helpful
-- links:
--
-- https://github.com/yesodweb/yesod/wiki/Sending-email
-- https://github.com/yesodweb/yesod/wiki/Serve-static-files-from-a-separate-domain
-- https://github.com/yesodweb/yesod/wiki/i18n-messages-in-the-scaffolding

getAppRoot :: Handler Text
getAppRoot = fmap (appRoot . appSettings) getYesod
