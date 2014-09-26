module Handler.Unsubscribe where

import Import
import Model.Subscription

getUnsubscribeR :: Token -> Handler Html
getUnsubscribeR token = do
    runDB $ do
        subscription <- getBy404 $ UniqueSubscription token

        unsubscribe $ entityKey subscription

    defaultLayout $ do
        setTitle "Unsubscribe"

        $(widgetFile "unsubscribe")
