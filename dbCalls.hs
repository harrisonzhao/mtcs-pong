{--
Interface to MySQL

cabal install mysql-simple
--}

import Database.MySQL.Simple
import Database.MySQL.Simple.QueryResults
import Database.MySQL.Simple.Result
import Control.Monad.Reader


connectInfo::ConnectInfo
connectInfo = ConnectInfo {
	connectHost = "localhost",
	connectPort = 3306,
	connectUser = "root",
	connectPassword = "",
	connectDatabase = "db",
	connectOptions = [],
	connectPath = "",
	connectSSL = Nothing }

data User = User {
	user :: String,
	gp :: Int,
	win :: Int,
	loss :: Int } deriving Show


insertUser :: String -> String -> String -> SqlCommand
insert name user pass = sqlCmd "insert into users (fullname, username,passwd) values (?, ?, ?)" (name,user,pass)




--main::IO()
main::IO Int()
main = do
	conn<-connect connectInfo
	--users<-demo conn
	--_<-putStrLn $ show users
	--return ()
	[Only i] <- query_ conn "select 2+2"
	return i


