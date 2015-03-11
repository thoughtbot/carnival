module Handler.Demo where

import Import

import Yesod.Default.Config

getDemoR :: Handler Html
getDemoR = do
    root <- fmap (appRoot . settings) getYesod
    Entity siteId _ <- runDB $ upsert (demoSite root) []

    defaultLayout $ do
        setTitle "Carnival Demo"
        $(widgetFile "demo")

demoSite :: Text -> Site
demoSite root = Site
    { siteName = "carnival-demo"
    , siteBaseUrl = root
    , siteRssAuthor = "Carnival Demo"
    , siteRssTitle = "Comments on Carnival Demo"
    , siteRssDescription = "Comments on Carnival Demo"
    , siteRssLanguage = "en-us"
    }
