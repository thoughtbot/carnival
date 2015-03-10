{-# LANGUAGE OverloadedStrings #-}
module Handler.CommentsSpec where

import TestHelper

main :: IO ()
main = hspec spec

spec :: Spec
spec = withApp $ do
    describe "GET CommentsR" $ do
        it "returns a list of comments by article" $ do
            Entity siteId _ <- createSite

            u <- createUser "1"
            c1 <- createComment (entityKey u) siteId "1" "1" "1"
            c2 <- createComment (entityKey u) siteId "1" "2" "2"
            c3 <- createComment (entityKey u) siteId "2" "1" "3"

            get $ CommentsR siteId

            valueEquals =<< commentsResponse siteId u [c1, c2, c3]

            getWithParams (CommentsR siteId) [("article", "1")]

            valueEquals =<< commentsResponse siteId u [c1, c2]

        it "returns comments for the correct site" $ do
            Entity sid1 _ <- createSite
            Entity sid2 _ <- createSite

            u <- createUser "1"
            c1 <- createComment (entityKey u) sid1 "1" "1" "1"
            c2 <- createComment (entityKey u) sid2 "1" "1" "1"
            c3 <- createComment (entityKey u) sid1 "1" "1" "2"
            c4 <- createComment (entityKey u) sid2 "1" "1" "2"

            get $ CommentsR sid1

            valueEquals =<< commentsResponse sid1 u [c1, c3]

            get $ CommentsR sid2

            valueEquals =<< commentsResponse sid2 u [c2, c4]

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
            userComment <- runDB $ buildUserComment siteId e u

            valueEquals $ object ["comment" .= userComment]

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
