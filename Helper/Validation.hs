module Helper.Validation where

import Import

runValidation :: Validation a -> a -> (a -> Handler b) -> Handler b
runValidation validate thing onSuccess =
    case validate thing of
        Right v -> onSuccess v
        Left es -> sendResponseStatus status400 $ object
            ["errors" .= map toJSON es]
