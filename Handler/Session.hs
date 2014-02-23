module Handler.Session where

import Import
import Yesod.Auth

getSessionR :: Handler Html
getSessionR = do
    mu <- maybeAuth
    sess <- getSession

    defaultLayout $ do
        setTitle "Session Info"
        $(widgetFile "session")
