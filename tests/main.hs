{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import Import
import Yesod.Default.Config
import Yesod.Test
import Test.Hspec (hspec)
import Application (makeFoundation)

import CreateCommentTest
import EditCommentTest
import LoadCommentsTest

main :: IO ()
main = do
    foundation <- makeFoundation =<< testConfig

    hspec $ yesodSpec foundation $ do
        createCommentSpecs
        editCommentSpecs
        loadCommentsSpecs

testConfig :: IO (AppConfig DefaultEnv Extra)
testConfig =
    Yesod.Default.Config.loadConfig
        $ (configSettings Testing)
        { csParseExtra = parseExtra }
