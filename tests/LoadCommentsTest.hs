{-# LANGUAGE OverloadedStrings #-}
module LoadCommentsTest
    ( loadCommentsSpecs
    ) where

import TestImport

loadCommentsSpecs :: Spec
loadCommentsSpecs =
    ydescribe "GET /api/v1/comments" $ do
        yit "returns a list of all comments in the system" $ do
            let comment = Comment "" ""
            insertComments [comment]

            get CommentsR

            bodyEquals' $ encode [comment]

        yit "can be limited by thread" $ do
            let thread = "A thread"
            let comment = Comment thread ""
            insertComments [comment, Comment "x" "", Comment "y" ""]

            getWithParams CommentsR [("thread", thread)]

            bodyEquals' $ encode [comment]
