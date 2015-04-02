module Model.UserSpec
    ( main
    , spec
    ) where

import TestImport
import Model.User
import Yesod.Auth (AuthenticationResult(..), Creds(..))
import qualified Database.Persist as DB

main :: IO ()
main = hspec spec

spec :: Spec
spec = withApp $ do
    describe "authenticateUser" $ do
        it "creates a new user with plugin, ident, and the free plan" $ do
            Entity planId _ <- runDB createFreePlan

            let creds = Creds
                    { credsPlugin = "dummy"
                    , credsIdent = "1"
                    , credsExtra = []
                    }

            Authenticated userId <- runDB $ authenticateUser' creds

            Just user <- runDB $ DB.get userId
            userPlugin user `shouldBe` credsPlugin creds
            userIdent user `shouldBe` credsIdent creds
            userPlan user `shouldBe` planId

        it "authenticates a known user by plugin/ident" $ do
            Entity userId user <- runDB $ createUser "1"
            let creds = Creds
                    { credsPlugin = userPlugin user
                    , credsIdent = userIdent user
                    , credsExtra = []
                    }

            Authenticated userId' <- runDB $ authenticateUser' creds

            userId' `shouldBe` userId

        context "from GitHub" $ do
            it "creates a user from the profile data" $ do
                void $ runDB createFreePlan
                let creds = Creds
                        { credsPlugin = "github"
                        , credsIdent = "1"
                        , credsExtra =
                            [ ("name", "foo")
                            , ("email", "bar@gmail.com")
                            ]
                        }

                Authenticated userId <- runDB $ authenticateUser' creds

                Just user <- runDB $ DB.get userId
                userName user `shouldBe` "foo"
                userEmail user `shouldBe` "bar@gmail.com"

            it "updates an existing user's profile data" $ do
                let creds = Creds
                        { credsPlugin = "github"
                        , credsIdent = "1"
                        , credsExtra =
                            [ ("name", "foo")
                            , ("email", "bar@gmail.com")
                            ]
                        }
                userId <- runDB $ do
                    Entity planId _ <- createFreePlan

                    insert User
                        { userName = ""
                        , userEmail = ""
                        , userPlugin = credsPlugin creds
                        , userIdent = credsIdent creds
                        , userPlan = planId
                        , userStripeId = Nothing
                        }

                Authenticated userId' <- runDB $ authenticateUser' creds

                userId' `shouldBe` userId

                Just user <- runDB $ DB.get userId
                userName user `shouldBe` "foo"
                userEmail user `shouldBe` "bar@gmail.com"

            it "errors if name is missing" $ do
                let creds = Creds
                        { credsPlugin = "github"
                        , credsIdent = "1"
                        , credsExtra = [("email", "foo@gmail.com")]
                        }

                ServerError msg <- runDB $ authenticateUser' creds

                msg `shouldBe` "github: missing key name"

            it "errors if email is missing" $ do
                let creds = Creds
                        { credsPlugin = "github"
                        , credsIdent = "1"
                        , credsExtra = [("name", "foo")]
                        }

                ServerError msg <- runDB $ authenticateUser' creds

                msg `shouldBe` "github: missing key email"

-- A synonym is required to fix m as App because we use the result as a concrete
-- AuthId App (i.e. UserId) so it can't be the generic AuthId m
authenticateUser' :: Creds App -> DB (AuthenticationResult App)
authenticateUser' = authenticateUser
