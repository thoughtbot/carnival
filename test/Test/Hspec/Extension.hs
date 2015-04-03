module Test.Hspec.Extension
    ( shouldHaveLength
    , shouldInclude
    ) where

import ClassyPrelude
import Test.Hspec.Expectations.Lifted

shouldHaveLength :: (MonadIO m, Show a) => [a] -> Int -> m ()
x `shouldHaveLength` n = x `shouldSatisfy` (== n) . length

shouldInclude :: (EqSequence a, MonadIO m, Show a) => a -> a -> m ()
x `shouldInclude` y = x `shouldSatisfy` (y `isInfixOf`)
