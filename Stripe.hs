module Stripe
    ( runStripe
    , subscribeToPlan
    , cancelSubscription
    ) where

import Import

import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Web.Stripe ((-&-))

import qualified Web.Stripe as S
import qualified Web.Stripe.Customer as S
import qualified Web.Stripe.Error as S
import qualified Web.Stripe.Subscription as S

type Stripe = ReaderT S.StripeConfig (ExceptT S.StripeError IO)

runStripe :: Stripe a -> Handler (Maybe a)
runStripe f = do
    stripeKey <- stripeKeysSecretKey . appStripeKeys <$> getYesod
    result <- liftIO $ runExceptT $ runReaderT f $ S.StripeConfig $ S.StripeKey $ encodeUtf8 stripeKey

    case result of
        Left err -> do
            $(logError) $ pack $ show err
            return Nothing
        Right x -> return $ Just x

subscribeToPlan :: S.TokenId
                -> S.Email
                -> S.PlanId
                -> Maybe S.CustomerId
                -> Stripe S.CustomerId
subscribeToPlan token email planId mstripeId = do
    stripeId <- findOrCreateCustomer token email mstripeId

    void $ request $ S.createSubscription stripeId planId

    return stripeId

cancelSubscription :: Maybe S.CustomerId -> Stripe ()
cancelSubscription Nothing = return ()
cancelSubscription (Just stripeId) = do
    subs <- S.list <$> (request $ S.getSubscriptions stripeId)

    forM_ subs $ \sub ->
        void $ request $ S.cancelSubscription stripeId (S.subscriptionId sub)
            -&- S.AtPeriodEnd False

findOrCreateCustomer :: S.TokenId -> S.Email -> Maybe S.CustomerId -> Stripe S.CustomerId
findOrCreateCustomer _ _ (Just stripeId) = return stripeId
findOrCreateCustomer token email _ =
    S.customerId <$> request (S.createCustomer -&- token -&- email)

request
    :: FromJSON (S.StripeReturn a)
    => S.StripeRequest a
    -> Stripe (S.StripeReturn a)
request f = do
    config <- ask
    result <- liftIO $ S.stripe config f
    either throwError return result
