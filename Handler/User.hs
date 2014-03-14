module Handler.User where

import Import
import Helper.Auth

getUserR :: Handler Value
getUserR = do
    user <- requireAuth_

    return $ object ["user" .= user]
