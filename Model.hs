{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}

module Model where

import Database.Persist
import Database.Persist.TH

import Data.Time.Clock

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

