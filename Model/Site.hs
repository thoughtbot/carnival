module Model.Site
    ( findSites
    , findMemberSite
    , createSite
    , destroySite
    , overSiteQuota
    , siteBranded
    , upsertDemoSite
    ) where

import Import
import Database.Esqueleto hiding ((==.), delete, on)

import qualified Database.Esqueleto as E

findSites :: UserId -> DB [Entity Site]
findSites userId = do
    memberships <- selectList [MembershipUser ==. userId] []

    let siteIds = map (membershipSite . entityVal) memberships

    selectList [SiteId <-. siteIds] []

findMemberSite :: SiteId -> UserId -> DB (Maybe Site)
findMemberSite siteId userId = do
    memberships <- selectList
        [ MembershipSite ==. siteId
        , MembershipUser ==. userId
        ] []

    case memberships of
        [] -> return Nothing
        _ -> get siteId

createSite :: UserId -> Site -> DB (Entity Site)
createSite userId site = do
    siteId <- insert site
    void $ insert $ Membership siteId userId

    return $ Entity siteId site

destroySite :: SiteId -> DB ()
destroySite siteId = do
    deleteWhere [CommentSite ==. siteId]
    deleteWhere [MembershipSite ==. siteId]
    delete siteId

overSiteQuota :: [a] -> Plan -> Bool
overSiteQuota xs Plan{..} = length xs >= planSiteQuota

siteBranded :: SiteId -> DB Bool
siteBranded siteId = null <$> nonBrandedPlans

  where
    nonBrandedPlans = select $
        from $ \(p `InnerJoin` u `InnerJoin` m) -> do
            E.on (m ^. MembershipUser E.==. u ^. UserId)
            E.on (p ^. PlanId E.==. u ^. UserPlan)
            where_ $
                (m ^. MembershipSite E.==. val siteId) &&.
                (p ^. PlanBranded E.==. val False)

            return p

upsertDemoSite :: Text -> DB SiteId
upsertDemoSite root = entityKey <$> upsert (demoSite root) []

demoSite :: Text -> Site
demoSite root = Site
    { siteName = "carnival-demo"
    , siteBaseUrl = root
    , siteLanguage = "en-us"
    }
