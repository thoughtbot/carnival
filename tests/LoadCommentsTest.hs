{-# LANGUAGE OverloadedStrings #-}
module LoadCommentsTest
    ( loadCommentsSpecs
    ) where

import TestImport

loadCommentsSpecs :: Spec
loadCommentsSpecs = do
    ydescribe "GET /api/v1/comments" $ do
        yit "returns a list of all comments in the system" $ do
            let (thread, body) = ("thread", "body")
            let comment = Comment thread body
            [commentId] <- insertComments [comment]

            get CommentsR

            bodyEquals' $ encode $ object
                ["comments" .= [(Entity commentId comment)]]

        yit "can be limited by thread" $ do
            let thread = "A thread"
            let comment = Comment thread ""
            (commentId:_) <- insertComments [comment, Comment "x" "", Comment "y" ""]

            getWithParams CommentsR [("thread", thread)]

            bodyEquals' $ encode $ object
                ["comments" .= [(Entity commentId comment)]]
