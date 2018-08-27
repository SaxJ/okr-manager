module Handler.Teams where

import Import

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
    team <- runDB $ get teamId
    returnJson team
