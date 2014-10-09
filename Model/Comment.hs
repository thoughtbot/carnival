module Model.Comment where

import Import

import Text.Blaze.Html (toMarkup)
import Text.Blaze.Renderer.String

import qualified Data.Text as T

renderMarkdown :: Comment -> Text
renderMarkdown = T.pack . renderMarkup . toMarkup . commentBody

validateComment :: Validation Comment
validateComment c
    | commentBody c == "" = Left ["Body cannot be blank"]
    | otherwise = Right c
