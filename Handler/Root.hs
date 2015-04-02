module Handler.Root where

import Import

import Yesod.Auth.OAuth2 (oauth2Url)

getRootR :: Handler Html
getRootR = do
    muser <- maybeAuth

    setUltDest SitesR

    defaultLayout $ do
        setTitle "Carnival"
        $(widgetFile "root")
