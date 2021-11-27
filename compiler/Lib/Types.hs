{-# LANGUAGE DeriveGeneric #-}

module Lib.Types
    ( Member(..)
    ) where

import GHC.Generics
import Data.Yaml
import Data.Time
import qualified Data.Text as T

data Member = Member
    { name :: T.Text
    , role :: T.Text
    , startYear :: Day
    , endYear :: Maybe Day
    , email :: T.Text
    , photo :: Maybe FilePath
    , current :: Maybe T.Text
    } deriving Generic

instance FromJSON Member