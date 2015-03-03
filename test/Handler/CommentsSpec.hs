{-# LANGUAGE OverloadedStrings #-}
module Handler.CommentsSpec where

import TestHelper
import qualified Database.Persist as DB

main :: IO ()
main = hspec spec

spec :: Spec
spec = withApp $ do
    describe "GET CommentsR" $ do
        it "returns a list of comments by article" $ do
            u <- createUser "1"
            c1 <- createComment (entityKey u) "1" "1" "1"
            c2 <- createComment (entityKey u) "1" "2" "2"
            c3 <- createComment (entityKey u) "2" "1" "3"
            Entity siteId _ <- createSite

            get $ CommentsR siteId

            valueEquals $ object
                ["comments" .= [ UserComment c1 u
                               , UserComment c2 u
                               , UserComment c3 u
                               ]]

            getWithParams (CommentsR siteId) [("article", "1")]

            valueEquals $ object
                ["comments" .= [ UserComment c1 u
                               , UserComment c2 u
                               ]]

    describe "POST CommentsR" $ do
        it "does not allow unauthenticated commenting" $ do
            Entity siteId _ <- createSite

            post $ CommentsR siteId

            statusIs 401


        it "allows commenting by authenticated users" $ do
            u <- createUser "1"
            Entity siteId _ <- createSite

            authenticateAs u

            postBody (CommentsR siteId) $ encode $ object
                [ "thread" .= ("The thread" :: Text)
                , "article_title" .= ("The article title" :: Text)
                , "article_author" .= ("John Smith" :: Text)
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

        it "validates against empty comment bodies" $ do
            u <- createUser "1"
            Entity siteId _ <- createSite

            authenticateAs u

            postBody (CommentsR siteId) $ encode $ object
                [ "thread" .= ("The thread" :: Text)
                , "article_url" .= ("The article" :: Text)
                , "article_title" .= ("The title" :: Text)
                , "article_author" .= ("John Smith" :: Text)
                , "body" .= ("" :: Text)
                ]

            statusIs 400

    describe "PUT CommentR" $ do
        it "only allows manipulating your own comments" $ do
            u1 <- createUser "1"
            u2 <- createUser "2"
            Entity cid1 _ <- createComment (entityKey u1) "1" "1" "1"
            Entity cid2 _ <- createComment (entityKey u2) "1" "1" "2"
            Entity siteId _ <- createSite

            authenticateAs u2

            putBody (CommentR siteId cid1) ""

            statusIs 403

            delete $ CommentR siteId cid1

            statusIs 403

            putBody (CommentR siteId cid2) $ encode $ object
                [ "thread" .= ("new thread" :: Text)
                , "article_title" .= ("new title" :: Text)
                , "article_author" .= ("John Smith" :: Text)
                , "article_url" .= ("new article" :: Text)
                , "body" .= ("new body" :: Text)
                ]

            statusIs 200

    describe "DELETE CommentR" $ do
        it "only allows deleting your own comments" $ do
            u1 <- createUser "1"
            u2 <- createUser "2"
            Entity cid1 _ <- createComment (entityKey u1) "1" "1" "1"
            Entity cid2 _ <- createComment (entityKey u2) "1" "1" "2"
            Entity siteId _ <- createSite

            authenticateAs u2

            delete $ CommentR siteId cid1

            statusIs 403

            delete $ CommentR siteId cid2

            statusIs 200

            mc <- runDB $ DB.get cid2
            assertEqual' mc Nothing
