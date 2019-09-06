{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.Result where

import Import

deleteResultR :: ResultId -> Handler Value
deleteResultR resultId = do
    result <- runDB $ get404 resultId
    objective <- runDB $ get404 (resultObjectiveId result)
    runDB $ delete resultId
    redirect (TeamR (objectiveTeamId objective))
