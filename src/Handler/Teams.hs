module Handler.Teams where

import Import

getTeamsR :: Handler Value
getTeamsR = do
    addHeader (pack "Access-Control-Allow-Origin") (pack "*")
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
