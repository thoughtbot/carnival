module Stripe
    ( runStripe
    , subscribeToPlan
    , cancelSubscription
    ) where

import Import

import qualified Web.Stripe as S
import qualified Web.Stripe.Customer as S
import qualified Web.Stripe.Subscription as S

runStripe :: FromJSON a => S.Stripe a -> Handler (Maybe a)
runStripe f = do
    stripeKey <- stripeKeysSecretKey . appStripeKeys <$> getYesod
    result <- liftIO $ S.stripe (S.StripeConfig $ encodeUtf8 stripeKey) f

    case result of
        Left err -> do
            $(logError) $ pack $ show err
            return Nothing
        Right x -> return $ Just x

subscribeToPlan :: S.TokenId
                -> S.Email
                -> S.PlanId
                -> Maybe S.CustomerId
                -> S.Stripe S.CustomerId
subscribeToPlan token email planId mstripeId = do
    stripeId <- findOrCreateCustomer token email mstripeId

    void $ S.createSubscription stripeId planId []

    return stripeId

cancelSubscription :: Maybe S.CustomerId -> S.Stripe ()
cancelSubscription Nothing = return ()
cancelSubscription (Just stripeId) = do
    subs <- S.list <$>
        S.getSubscriptions stripeId (Just 1) Nothing Nothing

    forM_ subs $ \sub ->
        void $ S.cancelSubscription stripeId (S.subscriptionId sub) False

findOrCreateCustomer :: S.TokenId -> S.Email -> Maybe S.CustomerId -> S.Stripe S.CustomerId
findOrCreateCustomer _ _ (Just stripeId) = return stripeId
findOrCreateCustomer token email _ = S.customerId <$> S.createCustomerBase
    Nothing         -- AccountBalance
    (Just token)    -- TokenId
    Nothing         -- CardNumber
    Nothing         -- ExpMonth
    Nothing         -- ExpYear
    Nothing         -- CVC
    Nothing         -- CouponId
    Nothing         -- Description
    (Just email)    -- Email
    Nothing         -- PlanId
    Nothing         -- Quantity
    Nothing         -- TrialPeriod
    []              -- MetaData
