module Handler.Purchase where

import Import
import Stripe

import qualified Web.Stripe.Customer as S

postPurchaseR :: PlanId -> Handler Html
postPurchaseR planId = do
    plan <- runDB $ get404 planId
    Entity userId user <- requireAuth

    token <- runInputPost $ ireq textField "stripeToken"
    mstripeId <- runStripe $
        subscribeToPlan (S.TokenId token) (planName plan) (userStripeId user)

    case mstripeId of
        Just stripeId -> do
            runDB $ update userId
                [ UserPlan =. planId
                , UserStripeId =. Just stripeId
                ]
            setMessage "Plan changed!"
            redirect RootR

        _ -> do
            setMessage "Oops! We ran into an error, please try again."
            redirect $ PlansR
