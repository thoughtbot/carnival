module TestHelpers.Assertions
    ( assertBool
    , assertEqual'
    , valueEquals
    ) where

import Yesod.Test
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (Value(..), eitherDecode)
import Network.Wai.Test (simpleBody)
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

-- | Like @bodyEquals@ but taking a @'Value'@ argument and comparing it against
--   the response body decoded as JSON.
--
--   > valueEquals $ object ["comments" .= comments]
--
valueEquals :: Value -> YesodExample site ()
valueEquals v = withResponse $
    either (const failure) (assertEqual' v) . eitherDecode . simpleBody

  where
    failure :: YesodExample site ()
    failure = liftIO $ HUnit.assertFailure "expected JSON response"
