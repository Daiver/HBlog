{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# OPTIONS_GHC -fno-warn-orphans       #-}

module Main where

import qualified Web.Scotty as S
import Control.Monad
import Control.Arrow
import Data.Monoid
import Data.Maybe
import Data.Either
import Control.Monad.IO.Class
import qualified Data.Map as Map

import Data.Text.Lazy(pack)

import Database.Persist.Sqlite 
import Database.Persist
import Database.Persist.TH
import Database.Persist.Sql
import Database.Persist.Types 

import Text.Blaze.Html.Renderer.String
import Text.Hamlet

import qualified Data.String.Utils as SUtils

import Data.Time.Clock

import Config
import Model

selectManyToMany targetFieldSelector sourceFieldSelector targetId = do
    proxyEntities <- selectList [targetFieldSelector ==. targetId] []
    res <- mapM (getJust . sourceFieldSelector . entityVal) proxyEntities
    return $ zipWith 
        (\prx rs -> Entity (sourceFieldSelector $ entityVal prx) rs) proxyEntities res

insertEntityIfNotExistsAndGetIds unique valueToInsert = 
    getBy unique >>= \tgs -> case tgs of
        Just val -> return $ entityKey val
        Nothing  -> return =<< insert $ valueToInsert

insertTagIfNotExistsAndGetTagsIds tagName = 
        insertEntityIfNotExistsAndGetIds (UniqueName tagName) (Tag tagName "")

addPostToDb caption text time tags = do 
    tagsIds <- mapM insertTagIfNotExistsAndGetTagsIds (words tags) 
    pid <- insert $ Post caption text time 
    mapM_ (insert . TagPost pid) tagsIds
    return pid

updatePostInDb pid caption text = do 
    {-tagsIds <- mapM insertIfNotExists (words tags) -}
    update pid [PostText =. text, PostCaption =. caption] 
    {-mapM_ (insert . TagPost pid) tagsIds-}
    return pid

runDb = runSqlite Config.sqliteDbName

persistInt64FromParam = PersistInt64 . fromIntegral . read 

main = do
    runDb $ runMigration migrateAll

    S.scotty 3000 $ do
        S.get "/" $ do
            allPostsEnt <- liftIO $ runDb $ selectList ([] :: [Filter Post]) [Desc PostCreated]
            tags <- liftIO . runDb $ mapM (selectManyToMany TagPostPostId tagPostTagId . entityKey) allPostsEnt
            let posts = zipWith (curry renderPost) allPostsEnt tags
            let content = $(shamletFile "./templates/list_of_posts.hamlet")
            S.html $ pack $ renderHtml $(shamletFile "./templates/base.hamlet")

        S.get "/bytag/:tagName" $ do
            tagName <- S.param "tagName"
            tagEntMaybe <- liftIO . runDb $ getBy $ UniqueName tagName
            case tagEntMaybe of
                Nothing -> S.html "404"
                Just tagEnt -> do
                    allPostsEnt <- liftIO $ runDb $ selectManyToMany TagPostTagId tagPostPostId $ entityKey tagEnt
                    tags <- liftIO . runDb $ mapM (selectManyToMany TagPostPostId tagPostTagId . entityKey) allPostsEnt
                    let posts = reverse $ zipWith (curry renderPost) allPostsEnt tags
                    let content = $(shamletFile "./templates/list_of_posts.hamlet")
                    S.html $ pack $ renderHtml $(shamletFile "./templates/base.hamlet")

        S.get "/post/:id" $ do
            id <- S.param "id"
            let key = toSqlKey $ read id
            t <- liftIO . runDb $ get (key::Key Post)
            case t of
                Nothing -> S.html "No post"
                Just post -> do
                    tags <- liftIO . runDb $ selectManyToMany TagPostPostId tagPostTagId key
                    let content = $(shamletFile "./templates/post.hamlet")
                    S.html $ pack $ renderHtml $(shamletFile "./templates/base.hamlet")

        S.get "/updatepost/:id" $ do
            id <- S.param "id"
            let key = toSqlKey $ read id
            t <- liftIO . runDb $ get (key::Key Post)
            case t of
                Nothing -> S.html "No post"
                Just post -> do
                    tagsTmp <- liftIO . runDb $ selectManyToMany TagPostPostId tagPostTagId key
                    let tags = unwords $ map (tagName . entityVal) tagsTmp
                    let content = $(shamletFile "./templates/postUpdate.hamlet")
                    S.html $ pack $ renderHtml $(shamletFile "./templates/base.hamlet")


        S.post "/addPost" $ do
            caption <- S.param "caption"
            text <- S.param "text"
            tags <- S.param "tags" 
            currTime <- liftIO getCurrentTime
            postId <- liftIO . runDb $ addPostToDb caption text currTime tags
            S.redirect $ pack ("/post/" ++ (show . unSqlBackendKey . unPostKey $ postId))

        S.post "/updatePost" $ do
            caption <- S.param "caption"
            id <- S.param "id"
            let key = toSqlKey $ read id
            t <- liftIO . runDb $ get (key::Key Post)
            case t of
                Nothing -> S.html "No post"
                Just post -> do
                    text <- S.param "text"
                    {-tags <- S.param "tags" -}
                    postId <- liftIO . runDb $ updatePostInDb key caption text 
                    S.redirect $ pack ("/post/" ++ (show . unSqlBackendKey . unPostKey $ postId))

        S.get "/deletepost/:id" $ do
            id <- S.param "id"
            let key = (toSqlKey $ read id) :: Key Post
            tagPostIds <- liftIO . runDb $ selectList [TagPostPostId ==. key] []
            mapM_ (liftIO . runDb . delete . entityKey) tagPostIds
            {-getBy unique >>= \tgs -> case tgs of-}

            liftIO . runDb $ delete key
            S.redirect $ pack Config.hostname

    where
        renderPost (postEnt, tags) = $(shamletFile "./templates/post.hamlet")
            where 
                post = entityVal postEnt
                id   = show . unSqlBackendKey . unPostKey $ entityKey postEnt
        postForm = $(shamletFile "./templates/postForm.hamlet")

