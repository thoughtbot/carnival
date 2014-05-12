module Handler.Session where

import Import

getSessionR :: Handler Html
getSessionR = do
    defaultLayout $ do
        setTitle "Session Info"
        $(widgetFile "session")
