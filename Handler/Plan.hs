module Handler.Plan where

import Import

getPlansR :: Handler Html
getPlansR = do
    plans <- runDB $ selectList [] [Asc PlanSort]
    muser <- maybeAuth

    defaultLayout $ do
        setTitle "Carnival - Plans"
        $(widgetFile "plans/index")

featuresList :: Plan -> Widget
featuresList plan = $(widgetFile "plans/features")

purchaseForm :: Maybe (Entity User) -> Entity Plan -> Widget
purchaseForm muser (Entity planId plan) = do
    stripeKey <- stripeKeysPublishableKey . appStripeKeys <$> getYesod

    $(widgetFile "plans/purchase")
