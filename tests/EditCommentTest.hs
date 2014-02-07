{-# LANGUAGE OverloadedStrings #-}
module EditCommentTest
    ( editCommentSpecs
    ) where

import TestImport hiding (get)
import Database.Persist (get)

editCommentSpecs :: Spec
editCommentSpecs =
    ydescribe "PUT /api/v1/comment/:comment_id" $ do
        yit "updates the given comment and responds 200" $ do
            clearComments
            let (thread, body, newBody) = ("A thread", "A body", "A new body")
                comment = Comment thread body

            [commentId] <- insertComments [comment]

            putBody (CommentR commentId) $ encode $ object
                [ "thread" .= thread
                , "body"   .= newBody
                ]

            Just c <- runDB $ get commentId
            assertEqual' (commentBody c) newBody
