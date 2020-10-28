{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}

module Protocol where

import Game
import Data.Serialize
import GHC.Generics

data Cmd = NewGame GamePreset Player
         | MakeMove Pos
         | Close
         deriving (Show, Generic)

deriving instance Generic GamePreset
deriving instance Generic Player
instance Serialize GamePreset
instance Serialize Player
instance Serialize Cmd
