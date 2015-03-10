{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
module NotificationSpec
    ( main
    , spec
    ) where

import TestImport
import Control.Applicative ((<$>))
import Model.Subscription
import Model.UserComment
import Notification

main :: IO ()
main = hspec spec

spec :: Spec
spec = withApp $
    describe "Notification" $
        describe "NewComment" $
            describe "notificationRecipients" $ do
                it "contains any users subscribed to the notification" $ do
                    siteId <- runDB $ insert buildSite
                    users1 <- runDB $ mapM createUser ["1", "2", "3"]
                    users2 <- runDB $ mapM createUser ["4", "5", "6"]
                    users3 <- runDB $ mapM createUser ["7", "8", "9"]
                    runDB $ do
                        mapM_ (subscribeUser siteId "1" "1") users1
                        mapM_ (subscribeUser siteId "1" "2") users2
                        mapM_ (subscribeUser siteId "2" "1") users3
                    commenter <- runDB $ createUser "10"
                    n <- runDB $ createNotification siteId "1" "2" commenter

                    rs <- runDB $ notificationRecipients n

                    map (userEmail . entityVal) users2
                        `shouldBe` map (userEmail . recipientUser) rs

                it "provides a valid token for unsubscribing" $ do
                    siteId <- runDB $ insert buildSite
                    user1 <- runDB $ createUser "1"
                    user2 <- runDB $ createUser "2"
                    runDB $ subscribeUser siteId "1" "1" user1
                    n <- runDB $ createNotification siteId "1" "1" user2
                    (r:_) <- runDB $ notificationRecipients n

                    get $ UnsubscribeR $ recipientToken r

                    runDB (notificationRecipients n) `shouldReturn` []

createSubscription :: Text -> Entity User -> DB ()
createSubscription name (Entity uid _) = subscribe name uid

createNotification :: SiteId -> Text -> Text -> Entity User -> DB Notification
createNotification siteId article thread u = do
    c <- createComment (entityKey u) siteId article thread ""

    NewComment <$> buildUserComment siteId c u

subscribeUser :: SiteId -> Text -> Text -> Entity User -> DB ()
subscribeUser siteId article thread eu = do
    notification <- createNotification siteId article thread eu

    createSubscription (notificationName notification) eu
