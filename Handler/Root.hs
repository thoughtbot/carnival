module Handler.Root where

import Import

getRootR :: Handler Html
getRootR = do
    muser <- maybeAuth

    defaultLayout $ do
        setTitle "Carnival"
        $(widgetFile "root")
