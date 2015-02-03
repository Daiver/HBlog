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

import qualified Data.Text.Lazy.IO as TLIO
import qualified Data.String.Utils as SUtils

import Data.Time.Clock

import HtmlTemplater

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
User
    name     String
    password String
    deriving Show

Tag
    name String
    description String
    UniqueName name

Post
    caption String
    text    String
    created UTCTime
    deriving Show

TagPost
    postId        PostId
    tagId         TagId
    UniqueTagPost tagId postId

|]

selectManyToMany targetFieldSelector sourceFieldSelector targetId = do
    proxyEntities <- selectList [targetFieldSelector ==. targetId] []
    res <- mapM (getJust . sourceFieldSelector . entityVal) proxyEntities
    return $ zipWith (\prx rs -> Entity (sourceFieldSelector $ entityVal prx) rs) proxyEntities res

dictForPost :: [Entity Tag] -> Entity Post -> Map.Map String String
dictForPost tags postEntity = Map.fromList [
        ("id", show . unSqlBackendKey . unPostKey $ entityKey postEntity), 
        ("caption", postCaption post), 
        ("text", postText post), 
        ("tags", tagsLine),
        ("created", show $ postCreated post)]
    where
        tagsLine = unwords $ map (tagName . entityVal) tags
        post = entityVal postEntity

addPostToDb caption text time tags = do 
        tagsIds <- mapM insertIfNotExists (words tags) 
        pid <- insert $ Post caption text time 
        mapM_ (insert . TagPost pid) tagsIds
        return pid
    where insertIfNotExists tag = 
            getBy (UniqueName tag) >>= \tgs -> case tgs of
                Just val -> return $ entityKey val
                Nothing  -> return =<< insert $ Tag tag ""

runDb = runSqlite "/home/daiver/jour.sqlite3"

persistInt64FromParam = PersistInt64 . fromIntegral . read 

main = do
    runDb $ runMigration migrateAll
    baseHtml  <- readFile "templates/base.html"
    indexHtml <- readFile "templates/index.html"
    formHtml  <- readFile "templates/newPostForm.html"
    postHtml  <- readFile "templates/post.html"

    let allPostsHtml  = renderTemplate (Map.fromList [("content", indexHtml)]) baseHtml
    let renderIndex   = renderTemplateFlipped allPostsHtml
    let renderPost    = renderTemplateFlipped postHtml
    let renderOnePost = renderTemplateFlipped baseHtml
    let postsToHtml = concatMap (renderPost . uncurry dictForPost) 

    S.scotty 3000 $ do
        S.get "/" $ do
            allPostsEnt <- liftIO $ runDb $ selectList ([] :: [Filter Post]) [Desc PostCreated]
            tags <- liftIO . runDb $ mapM (selectManyToMany TagPostPostId tagPostTagId . entityKey) allPostsEnt
            S.html $ pack $ renderIndex $
                Map.fromList [("posts", postsToHtml $ zip tags allPostsEnt), ("form", formHtml)]

        S.get "/bytag/:tagName" $ do
            tagName <- S.param "tagName"
            tagEntMaybe <- liftIO . runDb $ getBy $ UniqueName tagName
            case tagEntMaybe of
                Nothing -> S.html "404"
                Just tagEnt -> do
                    allPostsEnt <- liftIO $ runDb $ selectManyToMany TagPostTagId tagPostPostId $ entityKey tagEnt
                    tags <- liftIO . runDb $ mapM (selectManyToMany TagPostPostId tagPostTagId . entityKey) allPostsEnt
                    S.html $ pack $ renderIndex $
                        Map.fromList [("posts", postsToHtml $ zip tags allPostsEnt), ("form", formHtml)]

        S.get "/post/:id" $ do
            id <- S.param "id"
            let key = toSqlKey $ read id
            t <- liftIO . runDb $ get (key::Key Post)
            case t of
                Nothing -> S.html "No post"
                Just post -> do
                    tags <- liftIO . runDb $ selectManyToMany TagPostPostId tagPostTagId key
                    --S.html $ pack $ renderOnePost $ Map.fromList [("content", renderPost $ dictForPost tags (Entity key x))]
                    S.html $ pack $ renderHtml $ $(shamletFile "./templates/post.hamlet")

        S.post "/addPost" $ do
            caption <- S.param "caption"
            text <- S.param "text"
            tags <- S.param "tags" 
            currTime <- liftIO getCurrentTime
            postId <- liftIO . runDb $ addPostToDb caption text currTime tags
            S.redirect $ pack ("/post/" ++ (show . unSqlBackendKey . unPostKey $ postId))

        S.get "/deletepost/:id" $ do
            id <- S.param "id"
            let key = (toSqlKey $ read id) :: Key Post
            liftIO . runDb $ delete key
            S.redirect "http://127.0.0.1:3000/"
