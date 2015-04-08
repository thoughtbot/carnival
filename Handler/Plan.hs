module Handler.Plan where

import Import

getPlansR :: Handler Html
getPlansR = do
    plans <- runDB $ selectList [] [Asc PlanSort]
    Entity _ user <- requireAuth

    defaultLayout $ do
        setTitle "Carnival - Plans"
        $(widgetFile "plans/index")

featuresList :: Plan -> Widget
featuresList plan = $(widgetFile "plans/features")

purchaseForm :: User -> Entity Plan -> Widget
purchaseForm user (Entity planId plan) = do
    stripeKey <- stripeKeysPublishableKey . appStripeKeys <$> getYesod

    $(widgetFile "plans/purchase")
