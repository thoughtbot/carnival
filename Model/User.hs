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
authenticateUser Creds{..} = do
    muser <- getBy $ UniqueUser credsPlugin credsIdent

    let user = User
            <$> lookup "name" credsExtra
            <*> lookup "email" credsExtra
            <*> pure credsPlugin
            <*> pure credsIdent

    maybe (mapM insert user) (updateProfile user) muser

updateProfile :: Maybe User -> Entity User -> DB (Maybe UserId)
updateProfile muser (Entity uid _) = do
    mapM_ (replace uid) muser
    return $ Just uid
