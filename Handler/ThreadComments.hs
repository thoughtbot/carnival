module Handler.ThreadComments where

import Import
import qualified Data.Aeson as A

postThreadCommentsR :: Thread -> Handler ()
postThreadCommentsR thread = do
    comment <- parseJsonBody_ :: Handler Comment

    runDB $ insert comment

    sendResponseStatus status201 ("created" :: Text)
