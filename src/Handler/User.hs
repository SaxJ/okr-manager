module Handler.User where

import Import

getEditableR :: Handler Value
getEditableR = do
    allComments <- runDB $ selectList [] [Asc ObjectiveId]
    returnJson allComments
    --(_, user) <- requireAuthPair
    --members <- runDB $ selectList [UserId ==. userId user] []
    --returnJson members

postEditableR :: Handler Value
postEditableR = do
    allComments <- runDB $ selectList [] [Asc ObjectiveId]
    returnJson allComments
