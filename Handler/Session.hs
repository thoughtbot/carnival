module Handler.Session where

import Import
import Yesod.Auth

getSessionR :: Handler Html
getSessionR = do
    mu <- maybeAuth
    sess <- getSession

    defaultLayout $ do
        setTitle "Session Info"

        [whamlet|
            <div .container>
                <h1>Session Info

                <h3>Profile:

                $maybe Entity _ u <- mu
                    <ul>
                        <li><strong>Name</strong>: #{userFirstName u} #{userLastName u}
                        <li><strong>Email</strong>: #{userEmail u}
                $nothing
                    <p>Not authenticated

                <h3>Session:
                <pre>#{show sess}
        |]
