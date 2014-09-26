{-# OPTIONS_GHC -fno-warn-orphans #-}
module Model.User
    ( User(..)
    , userName
    , userGravatar
    , findUsers
    , findUsers'
    ) where

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

findUsers :: [UserId] -> DB [Entity User]
findUsers userIds = selectList [UserId <-. userIds] []

-- | Same as @findUsers@, but discards the @entityKey@
findUsers' :: [UserId] -> DB [User]
findUsers' = fmap (map entityVal) . findUsers
