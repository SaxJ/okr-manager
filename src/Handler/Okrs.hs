{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Okrs where

import Import
import Data.Maybe (fromJust)

getOkrsR :: Handler Html
getOkrsR = do
    (userId, user) <- requireAuthPair
    memberships <- runDB $ selectList [TeamMemberUserId ==. userId] []
    teams <- runDB $ selectList [TeamId <-. [tid | tid <- map (teamMemberTeamId.entityVal) memberships]] []
    objectives <- runDB $ selectList [ObjectiveTeamId <-. map entityKey teams] []
    results <- runDB $ selectList [ResultObjectiveId <-. map entityKey objectives] []

    let groupToTeam = \team obj -> (entityKey team) == (objectiveTeamId $ entityVal obj)
    let groupResults obj = (obj, rs) where rs = [r | r <- results, (resultObjectiveId $ entityVal r) == (entityKey obj)]
    let paired = [(team, map groupResults os) | team <- teams, let os = filter (groupToTeam team) objectives]

    defaultLayout $ do
        setTitle . toHtml $ userIdent user <> "'s OKRs"
        $(widgetFile "okrs")
