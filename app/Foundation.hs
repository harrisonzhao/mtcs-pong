module Foundation where

import Control.Concurrent.Chan
import Control.Concurrent.Map
import Control.Concurrent.STM
import Pong.Game

data Pong = Pong {
    userToGameId :: Map String Int,
    games :: TVar [IO Game],
    chatChan :: TChan Text,
    nextGameId :: TVar
}

--General Flow of Game Updates
