module Handler.Root where

import Import

import Yesod.Auth (Route(LoginR), maybeAuthId)

getRootR :: Handler Html
getRootR = do
    muser <- maybeAuthId

    defaultLayout $ do
        setTitle "Carnival"
        $(widgetFile "root")
