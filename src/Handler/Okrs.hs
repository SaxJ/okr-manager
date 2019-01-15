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
    (_, user) <- requireAuthPair

    objectives <- runDB $ selectList [] [Asc ObjectiveId, Asc ObjectiveTeamId]
    teams <- runDB $ selectList [] [Asc TeamId]
    results <- runDB $ selectList [] [Asc ResultId]

    let groupToTeam = \team obj -> (entityKey team) == (objectiveTeamId $ entityVal obj)
    let groupResults obj = (obj, rs) where rs = [r | r <- results, (resultObjectiveId $ entityVal r) == (entityKey obj)]
    let paired = [(team, map groupResults os) | team <- teams, let os = filter (groupToTeam team) objectives]

    defaultLayout $ do
        setTitle . toHtml $ userIdent user <> "'s OKRs"
        $(widgetFile "okrs")
