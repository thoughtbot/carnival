module Handler.Cancel where

import Import
import Stripe

postCancelR :: Handler Html
postCancelR = do
    plan <- runDB $ getBy404 $ UniquePlan freePlanId
    Entity userId user <- requireAuth

    result <- runStripe $ cancelSubscription $ userStripeId user

    case result of
        Just _ -> do
            runDB $ update userId [UserPlan =. entityKey plan]

            setMessage "Plan changed!"
            redirect RootR

        _ -> do
            setMessage "Oops! We ran into an error, please try again."
            redirect PlansR
