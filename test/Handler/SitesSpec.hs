{-# LANGUAGE OverloadedStrings #-}
module Handler.SitesSpec where

import TestHelper hiding (createSite)
import Model.Site
import qualified Database.Persist as DB

main :: IO ()
main = hspec spec

spec :: Spec
spec = withApp $ do
    describe "SitesR" $ do
        it "shows a list of sites for the correct user" $ do
            user1 <- createUser "1"
            user2 <- createUser "2"
            runDB $ mapM_ (createSite $ entityKey user1) $
                [ buildSite { siteName = "Site 1" }
                , buildSite { siteName = "Site 2" }
                ]

            authenticateAs user1
            get SitesR

            htmlAnyContain "li" "Site 1"
            htmlAnyContain "li" "Site 2"

            authenticateAs user2
            get SitesR

            htmlNoneContain "li" "Site 1"
            htmlNoneContain "li" "Site 2"

        it "allows for creation of a site" $ do
            user <- createUser "1"
            authenticateAs user

            postForm SitesR $ do
                byLabel "Name" "test-site"
                byLabel "Base URL" "http://example.com"
                byLabel "RSS Author" "Author"
                byLabel "RSS Title" "Title"
                byLabel "RSS Description" "Description"
                byLabel "RSS Language" "en-us"

            statusIs 303 -- redirect

            Just (Entity siteId site) <- runDB $ selectFirst [] []
            assertEqual' "test-site" $ siteName site
            assertEqual' "http://example.com" $ siteBaseUrl site

            get $ SiteR siteId

            bodyContains "test-site"

    describe "SiteR" $ do
        it "allows updating the site" $ do
            user <- createUser "1"
            site <- runDB $ createSite (entityKey user) $ buildSite
                { siteName = "site-name"
                , siteBaseUrl = "http://example.com"
                }

            authenticateAs user

            postForm (SiteR $ entityKey site) $ do
                byLabel "Name" "new-name"
                byLabel "Base URL" "http://new.example.com"
                byLabel "RSS Author" "untested"
                byLabel "RSS Title" "untested"
                byLabel "RSS Description" "untested"
                byLabel "RSS Language" "untested"

            statusIs 303 -- redirect

            Just site' <- runDB $ DB.get $ entityKey site
            assertEqual' "new-name" $ siteName site'
            assertEqual' "http://new.example.com" $ siteBaseUrl site'

        it "does not allow access to other users' sites" $ do
            user1 <- createUser "1"
            user2 <- createUser "2"
            site <- runDB $ createSite (entityKey user1) $ buildSite

            authenticateAs user2
            get $ SiteR $ entityKey site

            statusIs 404
