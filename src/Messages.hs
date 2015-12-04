{-# LANGUAGE DeriveGeneric #-}
module Messages where

import Data.Set as Set
import Data.Aeson
import Data.Text
import GHC.Generics

data MessageType = GameMsg
                 | ChatMsg
                 | ChallengeMsg
                 | ReadyMsg
                 | UsersOnlineMsg
                 | UsersInGameMsg 
                 | ChallengeExpMsg
                 | LeaveMsg deriving (Eq, Show, Generic)
instance ToJSON MessageType

data Challenge = Challenge {
    challenger :: Text,
    challenged :: Text
} deriving (Show, Generic)
instance ToJSON Challenge
instance FromJSON Challenge

data ChallengeExp = ChallengeExp {
    acceptingPlayer :: Text
} deriving (Show, Generic)
instance ToJSON ChallengeExp
instance FromJSON ChallengeExp

data Ready = Ready {
}deriving (Show, Generic)
instance ToJSON Ready
instance FromJSON Ready

data Chat = Chat{
    fromUser :: Text,
    chatData :: Text
}deriving (Show, Generic)
instance ToJSON Chat
instance FromJSON Chat

newLeaveMsg :: Text -> Message
newLeaveMsg username = newMessage LeaveMsg (toJSON username)

data UsersInGame = UsersInGame{
    usersIG :: [String]
}deriving (Show, Generic)
instance ToJSON UsersInGame
instance FromJSON UsersInGame

data UsersOnline = UsersOnline{
    usersOL :: Set.Set String
}deriving (Show, Generic)
instance ToJSON UsersOnline
instance FromJSON UsersOnline

newChatMsg :: Text -> Message
newChatMsg msg = newMessage ChatMsg (toJSON msg)

data Message = Message {
    msgType :: MessageType,
    msgData :: Value
} deriving (Show, Generic)
instance ToJSON Message

newMessage :: MessageType -> Value -> Message
newMessage mType mData =
    Message { msgType = mType
            , msgData = mData
            }
