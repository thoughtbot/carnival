{-# OPTIONS_GHC -fno-warn-orphans #-}
module Stripe.Instances where

import ClassyPrelude.Yesod
import Database.Persist.Sql

import qualified Web.Stripe.Customer as S

deriving instance PersistField S.PlanId
deriving instance PersistFieldSql S.PlanId

instance PersistField S.CustomerId where
    toPersistValue (S.CustomerId cid) = toPersistValue cid
    toPersistValue (S.ExpandedCustomer S.Customer{..}) =
        toPersistValue customerId
    toPersistValue (S.ExpandedCustomer S.DeletedCustomer{..}) =
        toPersistValue deletedCustomerId

    fromPersistValue (PersistText txt) = Right $ S.CustomerId txt
    fromPersistValue x = Left $ "Not a PersistText " ++ pack (show x)

instance PersistFieldSql S.CustomerId where
    sqlType _ = SqlString
