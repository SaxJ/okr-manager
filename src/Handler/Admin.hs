{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.Admin where

import Import
import Data.Maybe (fromMaybe)
import qualified Data.List as L
import Yesod.Form.Bootstrap4

data Tree = Empty | Tree (Entity Team) [Tree]

teamsToTree :: [Entity Team] -> Entity Team -> Tree
teamsToTree [] _ = Empty
teamsToTree teams t = Tree t $ map (teamsToTree teams) xs
    where
        xs = [a | a <- teams, (teamParent $ entityVal a) == (Just $ entityKey t)]

deleteFormClass :: String
deleteFormClass = "team-delete"

hasChildren :: Tree -> Bool
hasChildren Empty = False
hasChildren (Tree _ xs) = not $ null xs

treeWidget :: Tree -> WidgetT App m ()
treeWidget Empty = [whamlet|<h1>Nothing!|]
treeWidget tree@(Tree tm tms) =
    [whamlet|
        <ul .list-group.list-group-flush>
            <li .list-group-item>
                <form method=post action=@{TeamR $ entityKey tm}?_method=DELETE>
                    <button .close>
                        <span .close aria-label=Close style="color: red;">
                            <span aria-hidden="true">&times;</span>
                <div>
                    <a href="@{TeamR $ entityKey tm}">
                        <h3>
                            #{teamName $ entityVal tm}
                        <p>
                            #{fromMaybe "" $ teamDescription $ entityVal tm}
            $if hasChildren tree
                <li .list-group-item #team-list>
                    <ul .list-group>
                        $forall t <- tms
                            ^{treeWidget t}
    |]

data TeamForm = TeamForm
    { teamName' :: Text
    , teamDescription' :: Maybe Text
    , parent' :: Maybe (Entity Team)
    }

teamForm :: AForm Handler TeamForm
teamForm = TeamForm
    <$> areq textField (bfs ("Name" :: Text)) Nothing
    <*> aopt textField (bfs ("Description" :: Text)) Nothing
    <*> aopt (selectField teamOptions) (bfs ("Parent Team" :: Text)) Nothing
    where
        teamOptions = optionsPersist [] [Asc TeamName] teamName

getAdminR :: Handler Html
getAdminR = do
    (_, _) <- requireAuthPair
    ((result, formWidget), formEnctype) <- runFormPost $ renderBootstrap4 BootstrapBasicForm teamForm

    case result of
        FormSuccess formInput -> do
            let ent = Team (teamName' formInput) (teamDescription' formInput) (entityKey <$> parent' formInput)
            _ <- runDB $ insertUnique ent
            return ()
        _ -> return ()

    allTeams <- runDB $ selectList [] [Asc TeamName]
    let teamTree = teamsToTree allTeams $ L.head $ filter (isNothing . teamParent . entityVal) allTeams
    defaultLayout $ do
        $(widgetFile "admin")

postAdminR :: Handler Html
postAdminR = getAdminR

deleteAdminTeamR :: TeamId -> Handler Value
deleteAdminTeamR teamId = do
    _ <- runDB $ get404 teamId
    runDB $ delete teamId
    redirect AdminR
