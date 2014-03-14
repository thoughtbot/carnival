module Handler.Comments where

import Import
import Helper.Auth
import Helper.Comment
import Data.Maybe
import Helper.Request

postCommentsR :: Handler ()
postCommentsR = do
    allowCrossOrigin

    uid <- requireAuthId_
    _   <- runDB . insert . toComment uid =<< parseJsonBody_

    sendResponseStatus status201 ("CREATED" :: Text)

getCommentsR :: Handler Value
getCommentsR = do
    allowCrossOrigin

    filters  <- fmap toThreadFilter $ lookupGetParam "thread"
    comments <- runDB $ selectList filters []

    return $ object ["comments" .= comments]

    where
        toThreadFilter :: Maybe Text -> [Filter Comment]
        toThreadFilter = maybeToList . fmap (CommentThread ==.)
