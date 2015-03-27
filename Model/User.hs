{-# OPTIONS_GHC -fno-warn-orphans #-}
module Model.User
    ( User(..)
    , userGravatar
    , findUsers
    , findUsers'
    , reachedSiteQuota
    ) where

import Import
import Plan

import Network.Gravatar

import qualified Data.Text as T

instance ToJSON (Entity User) where
    toJSON (Entity uid u) = object
        [ "id" .= String (toPathPiece uid)
        , "name" .= userName u
        , "email" .= userEmail u
        , "gravatar_url" .= userGravatar u
        ]

userGravatar :: User -> Text
userGravatar = T.pack . gravatar def . userEmail

findUsers :: [UserId] -> DB [Entity User]
findUsers userIds = selectList [UserId <-. userIds] []

-- | Same as @findUsers@, but discards the @entityKey@
findUsers' :: [UserId] -> DB [User]
findUsers' = fmap (map entityVal) . findUsers

reachedSiteQuota :: Int -> User -> Bool
reachedSiteQuota siteCount = (siteCount >=) . planSiteCount . userPlan
