module Handler.Comments where

import Import
import Data.Maybe
import Helper.Request

postCommentsR :: Handler ()
postCommentsR = do
    allowCrossOrigin
    comment <- parseJsonBody_ :: Handler Comment
    _       <- runDB $ insert comment

    sendResponseStatus status201 ("CREATED" :: Text)

getCommentsR :: Handler Value
getCommentsR = do
    allowCrossOrigin
    filters <- fmap toThreadFilter $ lookupGetParam "thread"
    comments <- runDB $ selectList filters []

    return $ object ["comments" .= comments]

    where
        toThreadFilter :: Maybe Text -> [Filter Comment]
        toThreadFilter = maybeToList . fmap (CommentThread ==.)
