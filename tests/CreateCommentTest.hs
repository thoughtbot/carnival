{-# LANGUAGE OverloadedStrings #-}
module CreateCommentTest
    ( createCommentSpecs
    ) where

import TestImport
import qualified Data.ByteString.Lazy as BS

createCommentSpecs :: Spec
createCommentSpecs =
    ydescribe "POST /api/v1/threads/:thread_id/comments" $ do
        yit "responds with 201 created" $ do
            postBody (ThreadCommentsR thread) (commentJSON "")

            statusIs 201

        yit "creates a comment in the database" $ do
            postBody (ThreadCommentsR thread) (commentJSON "The body")

            ((Entity _ c):_) <- runDB $ selectList [] []

            assertEqual' "The body" (commentBody c)

thread :: Thread
thread = "abc123"

commentJSON :: ByteString -> ByteString
commentJSON body = BS.concat
    [ "{"
    , "  \"comment\": {"
    , "    \"body\": \"" `BS.append` body `BS.append` "\""
    , "  }"
    , "}"
    ]
