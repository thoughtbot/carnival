module Handler.Comment where

import Import

putCommentR :: CommentId -> Handler ()
putCommentR commentId = do
    comment <- parseJsonBody_ :: Handler Comment

    runDB $ update commentId (toUpdates comment)

    sendResponseStatus status200 ("OK" :: Text)

deleteCommentR :: CommentId -> Handler ()
deleteCommentR commentId = do
    runDB $ delete commentId

    sendResponseStatus status200 ("OK" :: Text)
