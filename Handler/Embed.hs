module Handler.Embed where

import Import
import Helper.Request

getEmbedR :: Handler Html
getEmbedR = do
    allowCrossOrigin
    embedLayout $ do
        toWidget $(luciusFile "embed")
        toWidget $(coffeeFile "embed/Article")
        toWidget $(coffeeFile "embed/Block")
        toWidget $(coffeeFile "embed/Comment")
        toWidget $(coffeeFile "embed/CommentForm")
        toWidget $(coffeeFile "embed/Comments")
        toWidget $(coffeeFile "embed/Carnival")
        toWidget $(coffeeFile "embed/initialize")
