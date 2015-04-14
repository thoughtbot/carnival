module Handler.Docs where

import Import
import Model.Site

getDocsR :: Handler Html
getDocsR = do
    root <- getAppRoot
    siteId <- runDB $ upsertDemoSite root

    defaultLayout $ do
        setTitle "Carnival - Documentation"
        $(widgetFile "docs")
