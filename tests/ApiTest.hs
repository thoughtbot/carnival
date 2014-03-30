{-# LANGUAGE OverloadedStrings #-}
module ApiTest (apiSpecs) where

import TestHelper

apiSpecs :: YesodSpec App
apiSpecs =
    ydescribe "Comments API" $ do
        yit "allows reading of comments by article" $ do
            clearTables

            Entity uid u <- createUser "1"
            c1 <- createComment uid "1" "1"
            c2 <- createComment uid "1" "2"
            c3 <- createComment uid "2" "3"

            get CommentsR

            bodyEquals' $ encode $ object
                ["comments" .= [ UserComment c1 u
                               , UserComment c2 u
                               , UserComment c3 u
                               ]]

            getWithParams CommentsR [("article", "1")]

            bodyEquals' $ encode $ object
                ["comments" .= [ UserComment c1 u
                               , UserComment c2 u
                               ]]

        yit "allows authorized commenting" $ do
            clearTables

            post CommentsR

            statusIs 401

            Entity uid u <- createUser "1"

            authenticateAs u

            postBody CommentsR $ encode $ object
                [ "thread"  .= ("The thread"  :: Text)
                , "article" .= ("The article" :: Text)
                , "body"    .= ("The body"    :: Text)
                ]

            statusIs 201

            ((Entity _ c):_) <- runDB $ selectList [] []

            assertEqual' uid $ commentUser c
            assertEqual' "The thread"  $ commentThread c
            assertEqual' "The article" $ commentArticle c
            assertEqual' "The body"    $ commentBody c

        yit "forbids manipulating other users' comments" $ do
            clearTables

            return ()
