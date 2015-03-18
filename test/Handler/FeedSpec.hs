module Handler.FeedSpec
    ( main
    , spec
    ) where

import TestImport

main :: IO ()
main = hspec spec

spec :: Spec
spec = withApp $
    describe "GET FeedR" $ do
        it "returns a feed of no items when there are no comments" $ do
            siteId <- runDB $ insert buildSite

            get $ FeedR siteId

            statusIs 200
            items <- htmlQuery "item"
            items `shouldHaveLength` 0

        it "includes at most 20 comments" $ do
            siteId <- runDB $ insert buildSite
            runDB $ do
                Entity uid _ <- createUser "1"
                forM_ ([1..21] :: [Int]) $ \_ -> do
                    void $ createComment uid siteId "1" "1" "1"

            get $ FeedR siteId

            items <- htmlQuery "item"
            items `shouldHaveLength` 20

        it "loads comments for the correct site" $ do
            siteId1 <- runDB $ insert buildSite { siteBaseUrl = "url1" }
            siteId2 <- runDB $ insert buildSite { siteBaseUrl = "url2" }
            runDB $ do
                Entity uid _ <- createUser "1"
                void $ createComment uid siteId1 "1" "1" "1"
                void $ createComment uid siteId2 "1" "1" "2"

            get $ FeedR siteId1

            items <- htmlQuery "item"
            items `shouldHaveLength` 1

            let item = unsafeHead items
            item `shouldInclude` "<guid>url1/1</guid>"

shouldHaveLength :: (MonadIO m, Show a) => [a] -> Int -> m ()
x `shouldHaveLength` n = x `shouldSatisfy` (== n) . length

shouldInclude :: (EqSequence a, MonadIO m, Show a) => a -> a -> m ()
x `shouldInclude` y = x `shouldSatisfy` (y `isInfixOf`)
