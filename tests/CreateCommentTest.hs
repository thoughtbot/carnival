{-# LANGUAGE OverloadedStrings #-}
module CreateCommentTest
    ( createCommentSpecs
    ) where

import TestImport

createCommentSpecs :: Spec
createCommentSpecs =
    ydescribe "POST /api/v1/threads/:thread_id/comments" $ do
        yit "responds with 201 and a Location header" $ do
            postBody ThreadCommentsR threadId commentJSON

            statusIs 201
            assertHeader "Location"

threadId :: Text
threadId = "abc123"

commentJSON :: ByteString
commentJSON = undefined
