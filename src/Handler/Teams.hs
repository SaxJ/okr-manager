{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.Teams where

import Import

getTeamsR :: Handler Value
getTeamsR = do
    allTeams <- runDB $ selectList [] [Asc TeamId]
    returnJson allTeams

postTeamsR :: Handler Value
postTeamsR = do
    team <- (requireJsonBody :: Handler Team)
    inserted <- runDB $ insertEntity team
    returnJson inserted

getTeamR :: TeamId -> Handler Html
getTeamR teamId = do
    (userId, user) <- requireAuthPair
    team <- runDB $ get404 teamId
    let teamAdmin = userAdmin

    defaultLayout $ do
        $(widgetFile "team")

getTeamObjectivesR :: TeamId -> Handler Value
getTeamObjectivesR teamId = do
    objectives <- runDB $ selectList [ObjectiveTeamId ==. teamId] []
    returnJson (objectives :: [Entity Objective])

postTeamObjectivesR :: TeamId -> Handler Value
postTeamObjectivesR team = do
    objective <- (requireJsonBody :: Handler Objective)
    objectiveId <- runDB $ insert objective
    runDB $ update objectiveId [ObjectiveTeamId =. team]
    returnJson objective
