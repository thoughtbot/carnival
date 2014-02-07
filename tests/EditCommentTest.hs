{-# LANGUAGE OverloadedStrings #-}
module EditCommentTest
    ( editCommentSpecs
    ) where

import TestImport

editCommentSpecs :: Spec
editCommentSpecs =
    ydescribe "PUT /api/v1/comment/:comment_id" $ do
        yit "updates the given comment and responds 200" $ do
            clearComments
            let (thread, body) = ("A thread", "A body")
                comment = Comment thread body

            cId <- runDB $ insert comment

            putBody (CommentR cId) $ encode $ object
                [ "thread" .= thread
                , "body"   .= ("A new body" :: Text)
                ]

            ((Entity _ c):_) <- runDB $ selectList [] []
            assertEqual' (commentBody c) "A new body"
