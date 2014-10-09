{-# OPTIONS_GHC -fno-warn-orphans #-}
module Model.User where

import Import

import Network.Gravatar

import qualified Data.Text as T

instance ToJSON (Entity User) where
    toJSON (Entity uid u) = object
        [ "id" .= String (toPathPiece uid)
        , "first_name" .= userFirstName u
        , "last_name" .= userLastName u
        , "email" .= userEmail u
        , "gravatar_url" .= userGravatar u
        ]

userName :: User -> Text
userName u = userFirstName u <> " " <> userLastName u

userGravatar :: User -> Text
userGravatar = T.pack . gravatar def . userEmail
