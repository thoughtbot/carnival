module Handler.User where

import Import
import Model.User ()

import Helper.Request

getUserR :: Handler Value
getUserR = do
    allowCrossOrigin

    user <- requireAuth

    return $ object ["user" .= user]
