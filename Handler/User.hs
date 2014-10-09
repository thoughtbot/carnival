module Handler.User where

import Import
import Model.User ()

import Helper.Auth
import Helper.Request

getUserR :: Handler Value
getUserR = do
    allowCrossOrigin

    user <- requireAuth_

    return $ object ["user" .= user]
