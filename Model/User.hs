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

data Profile = Profile
    { profileName :: Text
    , profileEmail :: Text
    }

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

authenticateUser :: AuthId m ~ UserId => Creds m -> DB (AuthenticationResult m)
authenticateUser creds@Creds{..} = do
    plan <- getEither "Free plan not found" $ UniquePlan freePlanId
    muser <- getBy $ UniqueUser credsPlugin credsIdent
    euserId <- mapM upsertUser $ credsToUser (entityKey <$> plan) creds

    return $ case (muser, euserId) of
        (Just user, _) -> Authenticated $ entityKey user
        (_, Right userId) -> Authenticated userId
        (_, Left err) -> ServerError $ credsPlugin ++ ": " ++ err

  where
    getEither msg = fmap (maybe (Left msg) Right) . getBy

upsertUser :: User -> DB UserId
upsertUser user = entityKey <$> upsert user
    [ UserName =. userName user
    , UserEmail =. userEmail user
    ]

credsToUser :: Either Text PlanId -> Creds m -> Either Text User
credsToUser eplanId Creds{..} = User
    <$> (profileName <$> eprofile)
    <*> (profileEmail <$> eprofile)
    <*> pure credsPlugin
    <*> pure credsIdent
    <*> eplanId
    <*> pure Nothing

  where
    eprofile = extraToProfile credsPlugin credsExtra

extraToProfile :: Text -> [(Text, Text)] -> Either Text Profile
extraToProfile "dummy" _ = Right dummyProfile
extraToProfile "github" extra = githubProfile extra
extraToProfile plugin _ = Left $ "Invalid plugin: " ++ plugin

dummyProfile :: Profile
dummyProfile = Profile "Dummy Login" "dummy@example.com"

githubProfile :: [(Text, Text)] -> Either Text Profile
githubProfile extra = Profile
    <$> lookupExtra "name" extra
    <*> lookupExtra "email" extra

lookupExtra :: Text -> [(Text, b)] -> Either Text b
lookupExtra k extra =
    maybe (Left $ "missing key " ++ k) Right $ lookup k extra
