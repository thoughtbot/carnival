{-# LANGUAGE OverloadedStrings #-}
module CreateCommentTest
    ( createCommentSpecs
    ) where

import TestImport

createCommentSpecs :: Spec
createCommentSpecs =
    ydescribe "POST /api/v1/threads/:thread_id/comments" $ do
        yit "responds with 201 created" $ do
            postBody CommentsR (encode $ Comment "" "")

            statusIs 201

        yit "creates a comment in the database" $ do
            let theThread = "A thread"
            let theBody = "A body"
            runDB $ deleteWhere ([] :: [Filter Comment])

            postBody CommentsR (encode $ Comment theThread theBody)

            ((Entity _ c):_) <- runDB $ selectList [] []
            assertEqual' theThread $ commentThread c
            assertEqual' theBody $ commentBody c
