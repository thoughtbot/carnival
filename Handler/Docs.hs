module Handler.Docs where

import Import

getDocsR :: Handler Html
getDocsR = do
    root <- getAppRoot
    Entity siteId _ <- runDB $ upsert (demoSite root) []

    defaultLayout $ do
        setTitle "Carnival - Documentation"
        addStylesheet $ StaticR css_bootstrap_css
        $(widgetFile "docs")

demoSite :: Text -> Site
demoSite root = Site
    { siteName = "carnival-demo"
    , siteBaseUrl = root
    , siteLanguage = "en-us"
    }
