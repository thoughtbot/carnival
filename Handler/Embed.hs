module Handler.Embed where

import Import

getEmbedR :: Handler Html
getEmbedR = do
    addHeader "Access-Control-Allow-Origin" "*"
    addHeader "Access-Control-Allow-Methods" "*"
    embedLayout $ do
        toWidget $(luciusFile "embed")
        toWidget $(coffeeFile "embed/Article")
        toWidget $(coffeeFile "embed/Block")
        toWidget $(coffeeFile "embed/Comment")
        toWidget $(coffeeFile "embed/CommentForm")
        toWidget $(coffeeFile "embed/Comments")
        toWidget $(coffeeFile "embed/Carnival")
        toWidget $(coffeeFile "embed/initialize")
