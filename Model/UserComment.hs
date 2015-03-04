module Model.UserComment where

import Import
import Model.Comment
import Model.User

import Control.Monad (forM)
import Data.List (find)
import Data.Maybe (catMaybes)

data UserComment = UserComment
    { userCommentComment :: Entity Comment
    , userCommentUser :: Entity User
    }

instance ToJSON UserComment where
    toJSON userComment = object
        [ "id" .= (String $ toPathPiece commentId)
        , "user_id" .= String (toPathPiece $ commentUser comment)
        , "user_name" .= userName user
        , "gravatar_url" .= userGravatar user
        , "article_url" .= commentArticleURL comment
        , "article_title" .= commentArticleTitle comment
        , "thread" .= commentThread comment
        , "body" .= unMarkdown (commentBody comment)
        , "body_html" .= String (renderMarkdown comment)
        ]

      where
        comment = entityVal ecomment
        commentId = entityKey ecomment
        ecomment = userCommentComment userComment
        user = entityVal $ userCommentUser userComment

-- Note: lives in DB b/c we will soon take a SiteId and lookup the Site
buildUserComment :: Entity Comment -> Entity User -> DB UserComment
buildUserComment ecomment euser =
    return UserComment
        { userCommentComment = ecomment
        , userCommentUser = euser
        }

findUserComments :: SiteId -> Maybe Text -> DB [UserComment]
findUserComments siteId marticle = do
    let filters = catMaybes
            [ Just $ CommentSite ==. siteId
            , fmap (CommentArticleURL ==.) marticle
            ]

    selectWithUsers filters []

findRecentUserComments :: DB [UserComment]
findRecentUserComments = selectWithUsers [] [Desc CommentCreated, LimitTo 20]

selectWithUsers :: [Filter Comment] -> [SelectOpt Comment] -> DB [UserComment]
selectWithUsers filters options = do
    comments <- selectList filters options
    users <- selectList
        [ UserId <-. map (commentUser . entityVal) comments
        ] []

    fmap catMaybes $ forM comments $ \c -> do
        let userId = commentUser $ entityVal c
            muser = find ((== userId) . entityKey) users

        maybe (return Nothing) (fmap Just . buildUserComment c) muser
