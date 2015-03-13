module Handler.CommentsSpec
    ( main
    , spec
    ) where

import TestImport
import Model.UserComment
import Data.Aeson (Value, (.=), encode, object)

main :: IO ()
main = hspec spec

spec :: Spec
spec = withApp $ do
    describe "GET CommentsR" $ do
        it "returns a list of comments by article" $ do
            siteId <- runDB $ insert buildSite

            u <- runDB $ createUser "1"
            c1 <- runDB $ createComment (entityKey u) siteId "1" "1" "1"
            c2 <- runDB $ createComment (entityKey u) siteId "1" "2" "2"
            c3 <- runDB $ createComment (entityKey u) siteId "2" "1" "3"

            get $ CommentsR siteId

            valueEquals =<< commentsResponse u [c1, c2, c3]

            getWithParams (CommentsR siteId) [("article", "1")]

            valueEquals =<< commentsResponse u [c1, c2]

        it "returns comments for the correct site" $ do
            sid1 <- runDB $ insert buildSite { siteBaseUrl = "http://ex1.com" }
            sid2 <- runDB $ insert buildSite { siteBaseUrl = "http://ex2.com" }

            u <- runDB $ createUser "1"
            c1 <- runDB $ createComment (entityKey u) sid1 "1" "1" "1"
            c2 <- runDB $ createComment (entityKey u) sid2 "1" "1" "1"
            c3 <- runDB $ createComment (entityKey u) sid1 "1" "1" "2"
            c4 <- runDB $ createComment (entityKey u) sid2 "1" "1" "2"

            get $ CommentsR sid1

            valueEquals =<< commentsResponse u [c1, c3]

            get $ CommentsR sid2

            valueEquals =<< commentsResponse u [c2, c4]

    describe "POST CommentsR" $ do
        it "does not allow unauthenticated commenting" $ do
            siteId <- runDB $ insert buildSite

            post $ CommentsR siteId

            statusIs 401

        it "allows commenting by authenticated users" $ do
            u <- runDB $ createUser "1"
            siteId <- runDB $ insert buildSite

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
            userComment <- runDB $ buildUserComment e u

            valueEquals $ object ["comment" .= userComment]

            commentUser c `shouldBe` entityKey u
            commentThread c `shouldBe` "The thread"
            commentArticleTitle c `shouldBe` "The article title"
            commentArticleURL c `shouldBe` "The article url"
            commentBody c `shouldBe` "The body"

        it "validates against empty comment bodies" $ do
            u <- runDB $ createUser "1"
            siteId <- runDB $ insert buildSite

            authenticateAs u

            postBody (CommentsR siteId) $ encode $ object
                [ "thread" .= ("The thread" :: Text)
                , "article_url" .= ("The article" :: Text)
                , "article_title" .= ("The title" :: Text)
                , "article_author" .= ("John Smith" :: Text)
                , "body" .= ("" :: Text)
                ]

            statusIs 400

commentsResponse :: Entity User -> [Entity Comment] -> YesodExample App Value
commentsResponse user comments = runDB $ do
    userComments <- forM comments $ \comment ->
        buildUserComment comment user

    return $ object ["comments" .= userComments]
