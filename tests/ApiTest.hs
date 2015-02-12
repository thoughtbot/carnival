{-# LANGUAGE OverloadedStrings #-}
module ApiTest (apiSpecs) where

import TestHelper
import qualified Database.Persist as DB

apiSpecs :: YesodSpec App
apiSpecs =
    ydescribe "Comments API" $ do
        yit "allows reading of comments by article" $ do
            u <- createUser "1"
            c1 <- createComment (entityKey u) "1" "1" "1"
            c2 <- createComment (entityKey u) "1" "2" "2"
            c3 <- createComment (entityKey u) "2" "1" "3"

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

            u <- createUser "1"

            authenticateAs u

            postBody CommentsR $ encode $ object
                [ "thread" .= ("The thread" :: Text)
                , "article_title" .= ("The article title" :: Text)
                , "article_url" .= ("The article url" :: Text)
                , "body" .= ("The body" :: Text)
                ]

            statusIs 201

            (e@(Entity _ c):_) <- runDB $ selectList [] []

            valueEquals $ object ["comment" .= UserComment e u]

            assertEqual' (entityKey u) $ commentUser c
            assertEqual' "The thread" $ commentThread c
            assertEqual' "The article title" $ commentArticleTitle c
            assertEqual' "The article url" $ commentArticleURL c
            assertEqual' "The body" $ commentBody c

        yit "forbids manipulating other users' comments" $ do
            u1  <- createUser "1"
            u2 <- createUser "2"
            Entity cid1 _  <- createComment (entityKey u1) "1" "1" "1"
            Entity cid2 _  <- createComment (entityKey u2) "1" "1" "2"

            authenticateAs u2

            putBody (CommentR cid1) ""

            statusIs 403

            delete $ CommentR cid1

            statusIs 403

            putBody (CommentR cid2) $ encode $ object
                [ "thread" .= ("new thread" :: Text)
                , "article_title" .= ("new title" :: Text)
                , "article_url" .= ("new article" :: Text)
                , "body" .= ("new body" :: Text)
                ]

            statusIs 200

            Just c <- runDB $ DB.get cid2

            assertEqual' "new body" $ commentBody c

            delete $ CommentR cid2

            mc <- runDB $ DB.get cid2

            assertEqual' mc Nothing

            statusIs 200

        yit "forbids posting empty comments" $ do
            u <- createUser "1"

            authenticateAs u

            postBody CommentsR $ encode $ object
                [ "thread" .= ("The thread" :: Text)
                , "article_url" .= ("The article" :: Text)
                , "article_title" .= ("The title" :: Text)
                , "body" .= ("" :: Text)
                ]

            statusIs 400
