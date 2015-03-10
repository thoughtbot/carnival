module Handler.EmbedSpec
    ( main
    , spec
    ) where

import TestImport

main :: IO ()
main = hspec spec

spec :: Spec
spec = withApp $
    describe "GET EmbedR" $
        it "renders successfully" $ do
            siteId <- runDB $ insert buildSite

            get $ EmbedR siteId

            statusIs 200
