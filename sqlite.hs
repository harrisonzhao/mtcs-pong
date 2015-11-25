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

--getUser :: Text -> Text -> Maybe Person
--getUser :: Text -> Text -> Maybe Person
--getUser un pw = do
--    usercheck <- getBy $ UsernameKey un
--    liftIO $ print usercheck
--Check if password matches input 

--createNew :: Text -> Text -> Text -> Maybe Person
--createNew un fn pw = do
--    newId <- insert $ Person fn un pw 0 0 0 

--userLoss :: Text -> Maybe Person
--userLoss un = do
--    updateWhere [PersonUsername ==. un] [PersonWin +=. 1]
--    updateWhere [PersonUsername ==. un] [PersonGamespl +=. 1]

--userWin :: Text -> Maybe Person   
--userWin un = do
--    updateWhere [PersonUsername ==. un] [PersonWin +=. 1]
--    updateWhere [PersonUsername ==. un] [PersonGamespl +=. 1]

--getTop :: Maybe Person
--getTop = do
--let sql = "select username, games_played, 
-- IF(win/loss is null,"undef",win/loss) as rating from users 
-- group by rating order by rating desc;"
-- rawQuery sql [] $$ CL.mapM_ (liftIO . print)

main :: IO ()
main = runSqlite ":memory:" $ do
--main = withMySQLConn defaultConnectInfo $ runSqlConn $ do
    runMigration migrateAll

    let s = "jdoe"
    johnId <- insert $ Person "John Doe" s  "password" 0 0 0 
    janeId <- insert $ Person  "Jane Doe" "jdoe2" "password" 0 0 0

    
    updateWhere [PersonUsername ==. s] [PersonWin +=. 1]


    john <- get johnId
    liftIO $ print (johnId :: Key Person)
    jane <- get janeId
    liftIO $ print (jane :: Maybe Person)
