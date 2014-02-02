{-# LANGUAGE OverloadedStrings #-}
module CreateCommentTest
    ( createCommentSpecs
    ) where

import TestImport

createCommentSpecs :: Spec
createCommentSpecs =
    ydescribe "POST /api/v1/comments" $ do
        yit "responds with 201 created" $ do
            postBody CommentsR $ commentJSON "" ""

            statusIs 201

        yit "creates a comment in the database" $ do
            clearComments
            let (thread, body) = ("A thread", "A body")

            postBody CommentsR $ commentJSON thread body

            ((Entity _ c):_) <- runDB $ selectList [] []
            assertEqual' (commentThread c) thread
            assertEqual' (commentBody c) body

commentJSON :: Text -> Text -> ByteString
commentJSON thread body = encode $ object ["thread" .= thread, "body" .= body]
