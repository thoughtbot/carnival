module Model.Subscription
    ( activeSubscriptions
    , activeSubscriptions'
    , subscribe
    , unsubscribe
    ) where

import Import

import Control.Monad (void)

activeSubscriptions :: Text -> DB [Entity Subscription]
activeSubscriptions name = selectList
    [ SubscriptionName ==. name
    , SubscriptionActive ==. True
    ] []

-- | Same as @activeSubscriptions@, but discards the @entityKey@
activeSubscriptions' :: Text -> DB [Subscription]
activeSubscriptions' = fmap (map entityVal) . activeSubscriptions

subscribe :: Text -> UserId -> DB ()
subscribe name userId = do
    token <- liftIO newToken

    void $ insert Subscription
        { subscriptionName = name
        , subscriptionUser = userId
        , subscriptionToken = token
        , subscriptionActive = True
        }

unsubscribe :: SubscriptionId -> DB ()
unsubscribe subscriptionId = update subscriptionId [SubscriptionActive =. False]
