module TestHelpers.Assertions
    ( assertBool
    , assertEqual'
    , valueEquals
    ) where

import Yesod.Test
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (Value(..))
import Data.Aeson.Encode (encodeToTextBuilder)
import Data.Text.Lazy.Builder (toLazyText)
import qualified Data.Text.Lazy as TL
import qualified Test.HUnit as HUnit

-- | Assert a value is true.
--
--   > assertBool "This thing should be true" thing
--
assertBool :: String -> Bool -> YesodExample site ()
assertBool msg = liftIO . HUnit.assertBool msg

-- | Like @'assertEqual'@ but no message argument.
--
--   > assertEqual' expected actual
--
assertEqual' :: (Eq a, Show a) => a -> a -> YesodExample site ()
assertEqual' expected actual = assertEqual msg actual expected

    where
        msg :: String
        msg = unlines
            [ "Values were not equal."
            , "  Expected: " ++ show expected
            , "    Actual: " ++ show actual
            ]

-- | Like @bodyEquals@ but taking a @'Value'@ argument and comparing its JSON
--   representation against the response body.
--
--   > valueEquals $ object ["comments" .= comments]
--
--   N.B. We avoid @Data.Aeson.encode@ here, because that does not escape HTML
--   entities and therefore the result may not match the response body (which
--   does).
--
valueEquals :: Value -> YesodExample site ()
valueEquals = bodyEquals . TL.unpack . toLazyText . encodeToTextBuilder
