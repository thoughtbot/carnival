{-# LANGUAGE OverloadedStrings #-}
module DeleteCommentTest
    ( deleteCommentSpecs
    ) where

import TestImport

deleteCommentSpecs :: Spec
deleteCommentSpecs =
    ydescribe "DELETE /api/v1/comment/:comment_id" $ do
        yit "deletes the given comment and responds 200" $ do
            clearComments
            commentId <- runDB $ insert $ Comment "" ""

            request $ do
                setMethod "DELETE"
                setUrl (CommentR commentId)

            comments <- runDB $ selectList [] [] :: Example [Entity Comment]

            assertBool "Expected no comments, found some" (null comments)
