{-# LANGUAGE OverloadedStrings #-}
module CreateCommentTest
    ( createCommentSpecs
    ) where

import TestImport

createCommentSpecs :: Spec
createCommentSpecs =
    ydescribe "POST /api/v1/threads/:thread_id/comments" $ do
        yit "responds with 201 created" $ do
            postBody (ThreadCommentsR thread) (encode $ Comment "")

            statusIs 201

        yit "creates a comment in the database" $ do
            runDB $ deleteWhere ([] :: [Filter Comment])

            postBody (ThreadCommentsR thread) (encode $ Comment "The body")

            ((Entity _ c):_) <- runDB $ selectList [] []
            assertEqual' "The body" (commentBody c)

thread :: Thread
thread = "abc123"
