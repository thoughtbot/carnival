{-# OPTIONS_GHC -fno-warn-orphans #-}
module Model.User
    ( User(..)
    , userGravatar
    , findUsers
    , findUsers'
    , authenticateUser

    -- Exported for tests
    , Profile(..)
    , dummyProfile
    ) where

import Import.NoFoundation

import Data.Aeson
import Network.Gravatar
import Yesod.Auth.GoogleEmail2

import qualified Data.Text as T
import qualified Data.ByteString.Lazy as BL

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
extraToProfile "googleemail2" extra = googleProfile extra
extraToProfile plugin _ = Left $ "Invalid plugin: " ++ plugin

dummyProfile :: Profile
dummyProfile = Profile "Dummy Login" "dummy@example.com"

githubProfile :: [(Text, Text)] -> Either Text Profile
githubProfile extra = Profile
    <$> lookupExtra "name" extra
    <*> lookupExtra "email" extra

googleProfile :: [(Text, Text)] -> Either Text Profile
googleProfile extra = Profile
  <$> (handleName =<< decodeText =<< lookupExtra "name" extra)
  <*> (handleEmails =<< decodeText =<< lookupExtra "emails" extra)

  where
    handleName :: Name -> Either Text Text
    handleName Name{..} =
        case (nameFormatted, nameGiven, nameFamily) of
            (Just formatted, _, _) -> Right $ formatted
            (_, Just given, Just family) -> Right $ given ++ " " ++ family
            (_, Just given, _) -> Right given
            (_, _, Just family) -> Right family
            _ -> Left "user has no name"

    handleEmails :: [Email] -> Either Text Text
    handleEmails [] = Left "user has no emails"
    handleEmails (email:_) = Right $ emailValue email

lookupExtra :: Text -> [(Text, b)] -> Either Text b
lookupExtra k extra =
    maybe (Left $ "missing key " ++ k) Right $ lookup k extra

decodeText :: FromJSON a => Text -> Either Text a
decodeText = first pack . eitherDecode . BL.fromStrict . encodeUtf8
