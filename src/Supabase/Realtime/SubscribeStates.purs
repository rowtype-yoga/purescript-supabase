module Supabase.Realtime.SubscribeStates where

import Prelude
import Unsafe.Coerce (unsafeCoerce)
import Unsafe.Reference (unsafeRefEq)
import Data.Maybe (Maybe(Nothing), Maybe, Maybe(Just))

data RealtimeSubscribeState = Subscribed | TimedOut | Closed | ChannelError
derive instance Eq RealtimeSubscribeState

toString :: RealtimeSubscribeState -> String
toString = case _ of
  Subscribed -> "SUBSCRIBED"
  TimedOut -> "TIMED_OUT"
  Closed -> "CLOSED"
  ChannelError -> "CHANNEL_ERROR"

fromString :: String -> Maybe RealtimeSubscribeState
fromString = case _ of
  "SUBSCRIBED" -> Just Subscribed
  "TIMED_OUT" -> Just TimedOut
  "CLOSED" -> Just Closed
  "CHANNEL_ERROR" -> Just ChannelError
  _ -> Nothing

