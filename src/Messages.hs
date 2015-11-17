{-# LANGUAGE DeriveGeneric #-}
module Messages where

import Data.Aeson
import Data.Text
import GHC.Generics

data MessageType = GameStatusMsg
                 | GameStateMsg
                 | ChatMsg
                 | ChallengeMsg deriving (Eq, Show)

data Challenge = Challenge {
    challenger :: String,
    challenged :: String
} deriving (Show, Generic)
instance ToJSON Challenge
instance FromJSON Challenge

newChatMsg :: Text -> Message
newChatMsg msg = newMessage ChatMsg (toJSON msg)

data Message = Message {
    msgType :: MessageType,
    msgData :: Value
} deriving (Show, Generic)

newMessage :: MessageType -> Value -> Message
newMessage mType mData =
    Message { msgType = mType
            , msgData = mData
            }
