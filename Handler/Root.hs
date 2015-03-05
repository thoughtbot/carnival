module Handler.Root where

import Import

import Yesod.Auth (Route(LoginR), maybeAuth)

getRootR :: Handler Html
getRootR = do
    muser <- maybeAuth

    defaultLayout $ do
        setTitle "Carnival"
        $(widgetFile "root")
