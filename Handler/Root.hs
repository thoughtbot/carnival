module Handler.Root where

import Import
import Model.Site

getRootR :: Handler Html
getRootR = do
    muser <- maybeAuth
    siteId <- runDB . upsertDemoSite =<< getAppRoot

    setUltDest SitesR

    defaultLayout $ do
        setTitle "Carnival"
        $(widgetFile "root")
