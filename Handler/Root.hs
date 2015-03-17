module Handler.Root where

import Import

getRootR :: Handler Html
getRootR = do
    muser <- maybeAuth

    setUltDest SitesR

    defaultLayout $ do
        setTitle "Carnival"
        $(widgetFile "root")
