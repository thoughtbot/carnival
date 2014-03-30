module TestHelpers.Assertions
    ( assertBool
    , assertEqual'
    , bodyEquals'
    ) where

import Yesod.Test
import Control.Monad.IO.Class (liftIO)
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Test.HUnit as HUnit

-- | Assert a value is true.
--
--   > assertBool "This thing should be true" thing
--
assertBool :: String -> Bool -> YesodExample site ()
assertBool msg = liftIO . HUnit.assertBool msg

-- | Like @'assertEqual'@ but no message argument.
--
--   > assertEqual' actual expected
--
assertEqual' :: (Eq a, Show a) => a -> a -> YesodExample site ()
assertEqual' actual expected = assertEqual msg actual expected

    where
        msg :: String
        msg = unlines
            [ "Values were not equal."
            , "  Expected: " ++ show expected
            , "    Actual: " ++ show actual
            ]


-- | Like @bodyEquals@ but taking a @'ByteString'@ argument (as is
--   returned by @'Data.Aeson.encode'@.
--
--   > bodyEquals' $ encode ["comments" .= comments]
--
bodyEquals' :: ByteString -> YesodExample site ()
bodyEquals' = bodyEquals . BS.unpack
