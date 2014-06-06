module Handler.Comment where

import Import
import Helper.Auth
import Helper.Comment
import Helper.Request

putCommentR :: CommentId -> Handler Value
putCommentR commentId = do
    allowCrossOrigin

    Entity uid u <- requireAuth_
    requireOwnComment uid commentId

    updateComment commentId uid u =<< requireJsonBody

deleteCommentR :: CommentId -> Handler ()
deleteCommentR commentId = do
    allowCrossOrigin

    uid <- requireAuthId_
    requireOwnComment uid commentId

    runDB $ delete commentId

    sendResponseStatus status200 ("DELETED" :: Text)
