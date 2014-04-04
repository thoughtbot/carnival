{-# LANGUAGE OverloadedStrings #-}
module TestHelpers.Auth
    ( authenticateAs
    ) where

import Model
import Foundation
import Yesod.Default.Config
import Yesod.Test

import qualified Data.Text as T

authenticateAs :: User -> YesodExample App ()
authenticateAs u = do
    testRoot <- fmap (appRoot . settings) $ getTestYesod

    let url = testRoot `T.append` "/auth/page/dummy"

    request $ do
        setMethod "POST"
        addPostParam "ident" $ userIdent u
        setUrl url
