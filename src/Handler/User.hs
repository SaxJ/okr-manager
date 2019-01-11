module Handler.User where

import Import
import Database.Persist.Types (PersistValue(PersistInt64), Key)

getEditableR :: Handler Value
getEditableR = do
    (_, user) <- requireAuthPair
    members <- runDB $ selectList [UserId ==. userId user] []
    returnJson members

postEditableR :: Handler Value
postEditableR = do
    allComments <- runDB $ selectList [] [Asc ObjectiveId]
    returnJson allComments
