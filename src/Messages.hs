{-# LANGUAGE DeriveGeneric #-}
module Messages where

import Data.Aeson
import Data.Text
import GHC.Generics

data MessageType = GameStatusMsg
                 | GameStateMsg
                 | ChatMsg
                 | ChallengeMsg deriving (Eq, Show, Generic)
instance ToJSON MessageType

data Challenge = Challenge {
    challenger :: Text,
    challenged :: Text
} deriving (Show, Generic)
instance ToJSON Challenge
instance FromJSON Challenge

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
