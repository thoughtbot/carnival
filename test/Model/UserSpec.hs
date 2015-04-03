{-# OPTIONS_GHC -fno-warn-orphans #-}
module Model.UserSpec
    ( main
    , spec
    ) where

import TestImport
import Model.User
import Yesod.Auth (AuthenticationResult(..), Creds(..))
import Yesod.Auth.Message (AuthMessage(..))
import qualified Database.Persist as DB

-- Required to derive for AuthenticationResult
deriving instance Eq AuthMessage
deriving instance Show AuthMessage

-- Required to use AuthenticationResult in assertions
deriving instance Eq (AuthenticationResult App)
deriving instance Show (AuthenticationResult App)

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

            result <- runDB $ authenticateUser' creds

            case result of
                Authenticated userId -> do
                    Just user <- runDB $ DB.get userId
                    userPlugin user `shouldBe` credsPlugin creds
                    userIdent user `shouldBe` credsIdent creds
                    userPlan user `shouldBe` planId

                x -> expectationFailure $ "unexpected " ++ show x

        it "authenticates a known user by plugin/ident" $ do
            Entity userId user <- runDB $ createUser "1"
            let creds = Creds
                    { credsPlugin = userPlugin user
                    , credsIdent = userIdent user
                    , credsExtra = []
                    }

            runDB (authenticateUser' creds) `shouldReturn` Authenticated userId

        context "from GitHub" $ do
            let creds = Creds
                    { credsPlugin = "github"
                    , credsIdent = "1"
                    , credsExtra =
                        [ ("name", "foo")
                        , ("email", "bar@gmail.com")
                        ]
                    }

            it "creates a user from the name/email values" $
                creds `shouldCreateWith` Profile "foo" "bar@gmail.com"

            it "updates an existing user from the name/email values" $
                creds `shouldUpdateWith` Profile "foo" "bar@gmail.com"

            it "errors if name is missing" $ do
                let creds' = creds { credsExtra = [("email", "")] }

                runDB (authenticateUser' creds')
                    `shouldReturn` ServerError "github: missing key name"

            it "errors if email is missing" $ do
                let creds' = creds { credsExtra = [("name", "")] }

                runDB (authenticateUser' creds')
                    `shouldReturn` ServerError "github: missing key email"

shouldCreateWith :: Creds App -> Profile -> YesodExample App ()
creds `shouldCreateWith` profile = do
    void $ runDB createFreePlan

    result <- runDB $ authenticateUser' creds

    case result of
        Authenticated userId -> do
            Just user <- runDB $ DB.get userId
            userName user `shouldBe` profileName profile
            userEmail user `shouldBe` profileEmail profile

        x -> expectationFailure $ "unexpected " ++ show x

shouldUpdateWith :: Creds App -> Profile -> YesodExample App ()
creds `shouldUpdateWith` profile = do
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

    runDB (authenticateUser' creds) `shouldReturn` Authenticated userId

    Just user <- runDB $ DB.get userId
    userName user `shouldBe` profileName profile
    userEmail user `shouldBe` profileEmail profile

-- A synonym is required to fix m as App because we use the result as a concrete
-- AuthId App (i.e. UserId) so it can't be the generic AuthId m
authenticateUser' :: Creds App -> DB (AuthenticationResult App)
authenticateUser' = authenticateUser
