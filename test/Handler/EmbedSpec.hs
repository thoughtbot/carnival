module Handler.EmbedSpec where

import TestHelper

main :: IO ()
main = hspec spec

spec :: Spec
spec = withApp $
    describe "GET EmbedR" $
        it "renders successfully" $ do
            Entity siteId _ <- createSite

            get $ EmbedR siteId

            statusIs 200
