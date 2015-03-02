{-# LANGUAGE OverloadedStrings #-}
module TestHelpers.Auth
    ( authenticateAs
    ) where

import Import
import Yesod.Default.Config
import Yesod.Test

import qualified Data.Text as T

authenticateAs :: Entity User -> YesodExample App ()
authenticateAs (Entity _ u) = do
    testRoot <- fmap (appRoot . settings) getTestYesod

    let url = testRoot `T.append` "/auth/page/dummy"

    request $ do
        setMethod "POST"
        addPostParam "ident" $ userIdent u
        setUrl url
