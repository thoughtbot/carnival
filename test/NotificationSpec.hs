{-# LANGUAGE OverloadedStrings #-}
module NotificationSpec where

import TestHelper
import Notification

main :: IO ()
main = hspec spec

spec :: Spec
spec = withApp $ do
    describe "Notification" $ do
        describe "NewComment" $ do
            describe "notificationRecipients" $ do
                it "contains any users subscribed to the notification" $ do
                    users1 <- mapM createUser ["1", "2", "3"]
                    users2 <- mapM createUser ["4", "5", "6"]
                    users3 <- mapM createUser ["7", "8", "9"]
                    mapM_ (subscribeUser "1" "1") users1
                    mapM_ (subscribeUser "1" "2") users2
                    mapM_ (subscribeUser "2" "1") users3
                    n <- createNotification "1" "2" =<< createUser "10"

                    rs <- runDB $ notificationRecipients n

                    assertEqual'
                        (map (userEmail . entityVal) users2)
                        (map (userEmail . recipientUser) rs)

                it "provides a valid token for unsubscribing" $ do
                    subscribeUser "1" "1" =<< createUser "1"
                    n <- createNotification "1" "1" =<< createUser "2"
                    (r:_) <- runDB $ notificationRecipients n

                    get $ UnsubscribeR $ recipientToken r

                    assertEqual' [] =<< runDB (notificationRecipients n)
