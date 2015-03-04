module Model.Site
    ( findSites
    , findMemberSite
    , createSite
    , destroySite
    ) where

import Import

import Control.Monad (void)

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
