module Model.UserSpec
    ( main
    , spec
    ) where

import TestImport
import Model.User
import Yesod.Auth (Creds(..))
import qualified Database.Persist as DB

main :: IO ()
main = hspec spec

spec :: Spec
spec = withApp $ do
    describe "authenticateUser" $ do
        it "creates a new user based on the given credentials" $ do
            void $ runDB createFreePlan

            let creds = Creds
                    { credsPlugin = "github"
                    , credsIdent = "1"
                    , credsExtra =
                        [ ("name", "foo")
                        , ("email", "bar@gmail.com")
                        ]
                    }

            Just userId <- runDB $ authenticateUser creds

            Just user <- runDB $ DB.get userId
            userPlugin user `shouldBe` "github"
            userIdent user `shouldBe` "1"
            userName user `shouldBe` "foo"
            userEmail user `shouldBe` "bar@gmail.com"

        it "updates an existing user based on the given credentials" $ do
            Entity userId user <- runDB $ createUser "1"
            let creds = Creds
                    { credsPlugin = userPlugin user
                    , credsIdent = userIdent user
                    , credsExtra =
                        [ ("name", "new")
                        , ("email", "new@gmail.com")
                        ]
                    }

            Just userId' <- runDB $ authenticateUser creds
            userId' `shouldBe` userId

            Just user' <- runDB $ DB.get userId'
            userPlugin user' `shouldBe` userPlugin user
            userIdent user' `shouldBe` userIdent user
            userName user' `shouldBe` "new"
            userEmail user' `shouldBe` "new@gmail.com"

        it "authenticates known users even if profile data is missing" $ do
            Entity userId user <- runDB $ createUser "1"
            let creds = Creds
                    { credsPlugin = userPlugin user
                    , credsIdent = userIdent user
                    , credsExtra = []
                    }

            (runDB $ authenticateUser creds) `shouldReturn` Just userId

        it "does not authenticate unknown users if profile data is missing" $ do
            let creds = Creds
                    { credsPlugin = "github"
                    , credsIdent = "1"
                    , credsExtra = []
                    }

            runDB (authenticateUser creds) `shouldReturn` Nothing
