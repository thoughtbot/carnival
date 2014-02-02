module Handler.Comments where

import Import

postCommentsR :: Handler ()
postCommentsR = do
    comment <- parseJsonBody_ :: Handler Comment
    _       <- runDB $ insert comment

    sendResponseStatus status201 ("created" :: Text)

getCommentsR :: Handler Value
getCommentsR = do
    mthread <- lookupGetParam "thread"

    let filters =
            case mthread of
            Just thread -> [CommentThread ==. thread]
            _           -> []

    comments <- runDB $ selectList filters []

    return $ object ["comments" .= comments]
