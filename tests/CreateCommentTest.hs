{-# LANGUAGE OverloadedStrings #-}
module CreateCommentTest
    ( createCommentSpecs
    ) where

import TestImport

createCommentSpecs :: Spec
createCommentSpecs =
    ydescribe "POST /api/v1/threads/:thread_id/comments" $ do
        yit "responds with 201 created" $ do
            postBody (ThreadCommentsR thread) commentJSON

            statusIs 201

thread :: Text
thread = "abc123"

commentJSON :: ByteString
commentJSON = undefined
