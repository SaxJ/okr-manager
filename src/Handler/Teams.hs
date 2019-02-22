module Handler.Teams where

import Import
import Database.Persist.Sql (fromSqlKey)

getTeamsR :: Handler Value
getTeamsR = do
    allComments <- runDB $ selectList [] [Asc TeamId]
    returnJson allComments

postTeamsR :: Handler Value
postTeamsR = do
    team <- (requireJsonBody :: Handler Team)
    inserted <- runDB $ insertEntity team
    returnJson inserted

getTeamR :: TeamId -> Handler Value
getTeamR teamId = do
    team <- runDB $ get404 teamId
    returnJson team

getTeamObjectivesR :: TeamId -> Handler Value
getTeamObjectivesR teamId = do
    objectives <- runDB $ selectList [ObjectiveTeamId ==. teamId] []
    returnJson (objectives :: [Entity Objective])

postTeamObjectivesR :: TeamId -> Handler Value
postTeamObjectivesR team = do
    objective <- (requireJsonBody :: Handler Objective)
    let updatedObjective = objective {teamId = team}
    objectiveId <- runDB $ insert updatedObjective
    runDB $ update objectiveId [ObjectiveTeamId =. team]
    returnJson objective
