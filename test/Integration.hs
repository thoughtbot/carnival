module Integration where

import TestImport
import IntegrationImport

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    user <- runIO $ runApp $ \app -> do
        wipeDB app
        runDBWithApp app $ createUser "1"

    describe "logging in to create a comment" $ session "" $ using Firefox $ do
        it "loads carnival" $ runWD $ do
            openPage "http://localhost:3000"

            clickOn $ ByCSS ".carnival-comment-indicator"
            clickOn $ ByCSS ".carnival-create"

            inModal $
                submit =<< fillIn (ByName "ident") (userIdent $ entityVal user)

            submit =<< fillIn (ByCSS ".carnival-body") "a comment"

            openPage "http://localhost:3000"
            clickOn $ ByCSS ".carnival-comment-indicator"
            clickOn $ ByCSS ".carnival-create"

            waitForText (ByCSS ".carnival-comment .carnival-body")
                `shouldReturn` "a comment"
