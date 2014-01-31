module Handler.ThreadComments where

import Import

postThreadCommentsR :: Thread -> Handler ()
postThreadCommentsR thread = do
    sendResponseStatus status201 ("created" :: Text)
