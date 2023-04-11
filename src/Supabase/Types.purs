module Supabase.Types where

import Prelude

foreign import data Client :: Type

foreign import data Channel :: Type

newtype ChannelName = ChannelName String
