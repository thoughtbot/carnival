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

commentsResponse :: SiteId
                 -> Entity User
                 -> [Entity Comment]
                 -> YesodExample App Value
commentsResponse siteId user comments = runDB $ do
    userComments <- forM comments $ \comment ->
        buildUserComment siteId comment user

    return $ object ["comments" .= userComments]
