{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
import           Control.Monad.IO.Class  (liftIO)
import           Database.Persist
import           Database.Persist.Sqlite
import           Database.Persist.Sql
--import           Database.Persist.MySql
import           Database.Persist.TH

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Person
    fullname String
    username String
    UsernameKey username
    pass String
    gamespl Int
    win Int
    loss Int
    deriving Show
|]

--getUser :: Text -> Maybe Person
getUser :: Text -> Maybe Person
getUser username = do
    usercheck <- getBy $ UsernameKey username
    liftIO $ print usercheck

main :: IO ()
main = runSqlite ":memory:" $ do
--main = withMySQLConn defaultConnectInfo $ runSqlConn $ do
    runMigration migrateAll

    let s = "jdoe"
    johnId <- insert $ Person "John Doe" s  "password" 0 0 0 
    janeId <- insert $ Person  "Jane Doe" "jdoe2" "password" 0 0 0

    getUser s

--    update $ \a -> do
--	set a [personLoss =.  1]
--	where_ (a ^. personUsername ==. "jdoe2")


    john <- get johnId
    liftIO $ print (johnId :: Key Person)
    jane <- get janeId
    liftIO $ print (jane :: Maybe Person)
