module Handler.Comment where

import Import
import Helper.Auth
import Helper.Comment
import Helper.Request

putCommentR :: CommentId -> Handler Value
putCommentR commentId = do
    allowCrossOrigin

    u <- requireAuth_
    requireOwnComment (entityKey u) commentId

    updateComment commentId u =<< requireJsonBody

deleteCommentR :: CommentId -> Handler ()
deleteCommentR commentId = do
    allowCrossOrigin

    uid <- requireAuthId_
    requireOwnComment uid commentId

    runDB $ delete commentId

    sendResponseStatus status200 ("DELETED" :: Text)
