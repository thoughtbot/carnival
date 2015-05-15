module Model.CommentSpec
    ( main
    , spec
    ) where

import TestImport
import Model.Comment
import Data.Time.Clock (addUTCTime)

main :: IO ()
main = hspec spec

spec :: Spec
spec = withApp $
    describe "deleteStaleComments" $
        it "deletes comments older than a week for the given site" $ do
            siteId <- createSiteWithUrl "http://example.com/site"
            otherSiteId <- createSiteWithUrl "http://example.com/otherSite"
            Entity u _ <- runDB $ createUser "example"
            runDB $ do
                setCreatedAgeInDays 6 =<<
                    createComment u siteId "x" "x" "recent"
                setCreatedAgeInDays 8 =<<
                    createComment u siteId "x" "abc" "old"
                setCreatedAgeInDays 8 =<<
                    createComment u otherSiteId "x" "x" "other site"

            runDB $ deleteStaleComments siteId

            result <- runDB
                $ map (commentBody . entityVal)
                <$> selectList [] [Desc CommentBody]

            result `shouldBe` ["recent", "other site"]

createSiteWithUrl :: Text -> YesodExample App SiteId
createSiteWithUrl u = runDB $ entityKey <$> insertEntity site
  where
    site = buildSite { siteBaseUrl = u }

setCreatedAgeInDays :: Integer -> Entity Comment -> DB ()
setCreatedAgeInDays days (Entity cid _) = do
    now <- liftIO getCurrentTime
    void $ update cid $ [CommentCreated =. addUTCTime age now]
  where
    age = fromInteger $ -days * 60 * 60 * 24
