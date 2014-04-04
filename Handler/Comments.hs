module Handler.Comments where

import Import
import Helper.Auth
import Helper.Comment
import Helper.Request
import Control.Monad (forM)
import Data.Maybe (catMaybes)
import Database.Persist (EntityField(..))

postCommentsR :: Handler ()
postCommentsR = do
    allowCrossOrigin

    uid <- requireAuthId_
    _   <- runDB . insert . toComment uid =<< parseJsonBody_

    sendResponseStatus status201 ("CREATED" :: Text)

getCommentsR :: Handler Value
getCommentsR = do
    allowCrossOrigin

    filters <- toFilters
        [ ("thread" , CommentThread)
        , ("article", CommentArticle)
        ]

    comments <- runDB $ mapM addUserInfo =<< selectList filters []

    return $ object ["comments" .= catMaybes comments]

    where
        -- Take a param "specification" (a list of param-name, comment field
        -- pairs) and return a list of filters for selecting comments whose
        -- fields match the values of those request parameters.
        toFilters :: [(Text, EntityField Comment Text)] -> Handler [Filter Comment]
        toFilters spec = fmap catMaybes $ forM spec $ \(n, c) ->
            fmap (fmap (c ==.)) $ lookupGetParam n

        -- N.B. This is N+1. Consider rewriting as a join, IFF this
        -- becomes an issue.
        addUserInfo :: Entity Comment -> YesodDB App (Maybe UserComment)
        addUserInfo e@(Entity _ c) = do
            u <- get $ commentUser c
            return $ fmap (UserComment e) u
