module Handler.Comments where

import Import

postCommentsR :: Handler ()
postCommentsR = do
    comment <- parseJsonBody_ :: Handler Comment
    _       <- runDB $ insert comment

    sendResponseStatus status201 ("created" :: Text)
