module Handler.Home where

import Import
import Yesod.Auth

getHomeR :: Handler Html
getHomeR = do
    mu <- maybeAuth
    sess <- getSession

    defaultLayout $ do
        setTitle "Home"

        [whamlet|
            <h1>Hi

            <p>#{show sess}

            $maybe Entity _ u <- mu
                <p>#{userFirstName u} #{userLastName u}
                <p>#{userEmail u}
            $nothing
                <p>Not authenticated
        |]
