module Model.UserComment where

import Import
import Model.Comment
import Model.User

data UserComment = UserComment
    { userCommentSite :: Entity Site
    , userCommentComment :: Entity Comment
    , userCommentUser :: Entity User
    }

userCommentCreated :: UserComment -> UTCTime
userCommentCreated = commentCreated . entityVal . userCommentComment

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

buildUserComment :: Entity Comment -> Entity User -> DB UserComment
buildUserComment ecomment euser = do
    site <- get404 siteId

    return UserComment
        { userCommentSite = Entity siteId site
        , userCommentComment = ecomment
        , userCommentUser = euser
        }

  where
    siteId = commentSite $ entityVal ecomment

findUserComments :: SiteId -> Maybe Text -> DB [UserComment]
findUserComments siteId marticle = do
    let filters = catMaybes
            [ Just $ CommentSite ==. siteId
            , fmap (CommentArticleURL ==.) marticle
            ]

    selectWithUsers filters [Asc CommentCreated]

findRecentUserComments :: SiteId -> DB [UserComment]
findRecentUserComments siteId =
    selectWithUsers [CommentSite ==. siteId] [Desc CommentCreated, LimitTo 20]

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
