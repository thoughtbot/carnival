{-# LANGUAGE CPP #-}
#if INTEGRATION
module Main where

import ClassyPrelude
import Test.Hspec

import Integration (spec)

main :: IO ()
main = hspec spec
#else
{-# OPTIONS_GHC -F -pgmF hspec-discover #-}
#endif
