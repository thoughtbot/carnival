module Handler.EmbedSpec where

import TestHelper

main :: IO ()
main = hspec spec

spec :: Spec
spec = withApp $
    describe "GET EmbedR" $
        it "renders successfully" $ do
            get EmbedR

            statusIs 200
