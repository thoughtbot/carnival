{-# LANGUAGE OverloadedStrings #-}
module ApiTest (apiSpecs) where

import TestHelper
import qualified Database.Persist as DB

apiSpecs :: YesodSpec App
apiSpecs =
    ydescribe "Comments API" $ do
        yit "allows reading of comments by article" $ do
            Entity uid u <- createUser "1"
            c1 <- createComment uid "1" "1"
            c2 <- createComment uid "1" "2"
            c3 <- createComment uid "2" "3"

            get CommentsR

            valueEquals $ object
                ["comments" .= [ UserComment c1 u
                               , UserComment c2 u
                               , UserComment c3 u
                               ]]

            getWithParams CommentsR [("article", "1")]

            valueEquals $ object
                ["comments" .= [ UserComment c1 u
                               , UserComment c2 u
                               ]]

        yit "allows authorized commenting" $ do
            post CommentsR

            statusIs 401

            Entity uid u <- createUser "1"

            authenticateAs u

            postBody CommentsR $ encode $ object
                [ "thread" .= ("The thread"  :: Text)
                , "articleTitle" .= ("The Title"   :: Text)
                , "article" .= ("The article" :: Text)
                , "body" .= ("The body"    :: Text)
                ]

            statusIs 201


            (e@(Entity _ c):_) <- runDB $ selectList [] []

            valueEquals $ object ["comment" .= UserComment e u]

            assertEqual' uid $ commentUser c
            assertEqual' "The thread"  $ commentThread c
            assertEqual' "The Title"   $ commentArticleTitle c
            assertEqual' "The article" $ commentArticleURL c
            assertEqual' "The body"    $ commentBody c

        yit "forbids manipulating other users' comments" $ do
            Entity uid1 _  <- createUser "1"
            Entity uid2 u2 <- createUser "2"
            Entity cid1 _  <- createComment uid1 "1" "1"
            Entity cid2 _  <- createComment uid2 "1" "2"

            authenticateAs u2

            putBody (CommentR cid1) ""

            statusIs 403

            delete $ CommentR cid1

            statusIs 403

            putBody (CommentR cid2) $ encode $ object
                [ "thread" .= ("new thread"  :: Text)
                , "articleTitle" .= ("new title"   :: Text)
                , "article" .= ("new article" :: Text)
                , "body" .= ("new body"    :: Text)
                ]

            statusIs 200

            Just c <- runDB $ DB.get cid2

            assertEqual' "new body" $ commentBody c

            delete $ CommentR cid2

            mc <- runDB $ DB.get cid2

            assertEqual' mc Nothing

            statusIs 200

        yit "forbids posting empty comments" $ do
            Entity _ u <- createUser "1"

            authenticateAs u

            postBody CommentsR $ encode $ object
                [ "thread" .= ("The thread"  :: Text)
                , "articleTitle" .= ("The article" :: Text)
                , "body" .= ("" :: Text)
                ]

            statusIs 400
