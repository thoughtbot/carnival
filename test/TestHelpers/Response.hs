{-# LANGUAGE OverloadedStrings #-}
module TestHelpers.Response
    ( commentsResponse
    ) where

import Model
import Model.UserComment
import Foundation

import TestHelpers.DB

import Control.Monad (forM)
import Data.Aeson (Value, object, (.=))
import Database.Persist
import Yesod.Test

commentsResponse :: Entity User -> [Entity Comment] -> YesodExample App Value
commentsResponse user comments = runDB $ do
    userComments <- forM comments $ \comment ->
        buildUserComment comment user

    return $ object ["comments" .= userComments]
