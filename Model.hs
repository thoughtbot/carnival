module Model where

import Prelude
import Yesod
import Data.Text (Text)
import Database.Persist.Quasi
import Data.Typeable (Typeable)

import Yesod.Markdown
import Text.Blaze.Renderer.String
import qualified Data.Text as T

share [mkPersist sqlOnlySettings, mkMigrate "migrateAll"]
    $(persistFileWith lowerCaseSettings "config/models")

instance ToJSON (Entity User) where
    toJSON (Entity uid u) = object
        [ "id"         .= (String $ toPathPiece uid)
        , "first_name" .= userFirstName u
        , "last_name"  .= userLastName u
        , "email"      .= userEmail u
        ]

instance ToJSON (Entity Comment) where
    toJSON (Entity cid c) = object
        [ "id"        .= (String $ toPathPiece cid)
        , "user_id"   .= (String $ toPathPiece $ commentUser c)
        , "article"   .= commentArticle c
        , "thread"    .= commentThread c
        , "body"      .= (unMarkdown $ commentBody c)
        , "body_html" .= (String $ renderMarkdown c)
        ]

renderMarkdown :: Comment -> Text
renderMarkdown = T.pack . renderMarkup . markdownToHtml . commentBody
