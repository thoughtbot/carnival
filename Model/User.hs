{-# OPTIONS_GHC -fno-warn-orphans #-}
module Model.User
    ( User(..)
    , userGravatar
    , findUsers
    , findUsers'
    , authenticateUser
    ) where

import Import.NoFoundation

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

authenticateUser :: Creds m -> DB (Maybe UserId)
authenticateUser = mapM upsertUser . credsToUser

upsertUser :: User -> DB UserId
upsertUser user = entityKey <$> upsert user
    [ UserName =. userName user
    , UserEmail =. userEmail user
    ]

credsToUser :: Creds m -> Maybe User
credsToUser Creds{..} = User
    <$> lookup "name" credsExtra
    <*> lookup "email" credsExtra
    <*> pure credsPlugin
    <*> pure credsIdent
