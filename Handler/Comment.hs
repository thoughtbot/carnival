module Handler.Comment where

import Import
import Helper.Auth
import Helper.Comment
import Helper.Request

putCommentR :: CommentId -> Handler ()
putCommentR commentId = do
    allowCrossOrigin

    uid <- requireAuthId_
    requireOwnComment uid commentId

    runDB $ updateComment commentId uid =<< requireJsonBody

    sendResponseStatus status200 ("UPDATED" :: Text)

deleteCommentR :: CommentId -> Handler ()
deleteCommentR commentId = do
    allowCrossOrigin

    uid <- requireAuthId_
    requireOwnComment uid commentId

    runDB $ delete commentId

    sendResponseStatus status200 ("DELETED" :: Text)
