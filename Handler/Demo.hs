module Handler.Demo where

import Import

getDemoR :: Handler Html
getDemoR = do
    root <- getAppRoot
    Entity siteId _ <- runDB $ upsert (demoSite root) []

    defaultLayout $ do
        setTitle "Carnival Demo"
        $(widgetFile "demo")

demoSite :: Text -> Site
demoSite root = Site
    { siteName = "carnival-demo"
    , siteBaseUrl = root
    , siteLanguage = "en-us"
    }
