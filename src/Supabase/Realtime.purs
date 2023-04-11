module Supabase.Realtime where

import Prelude
import Effect.Uncurried (EffectFn1, EffectFn2, EffectFn3, EffectFn4, mkEffectFn1, mkEffectFn2, runEffectFn2, runEffectFn3, runEffectFn4)
import Supabase.Types (Channel)
import Control.Promise (Promise, toAffE)
import Effect.Aff (Aff)
import Effect (Effect)
import Unsafe.Coerce (unsafeCoerce)
import Supabase.Realtime.ListenType (RealtimeListenType)
import Supabase.Realtime.SubscribeStates (RealtimeSubscribeState, RealtimeSubscribeState(ChannelError))
import Effect.Exception (Error, error)
import Untagged.Union (UndefinedOr, uorToMaybe)
import Data.Time.Duration (Milliseconds)
import Data.Maybe (Maybe, Maybe(Nothing), Maybe(Just))
import Supabase.Realtime.SubscribeStates (fromString) as SubscribeState
import Control.Monad.Error.Class (throwError)

data RealtimeResponse = SendingOK | SendingTimedOut | SendingRateLimited

foreign import sendImpl :: forall i. EffectFn2 { | i } Channel (Promise String)

send :: forall i. { | i } -> Channel -> Aff RealtimeResponse
send input channel = do
  res <- runEffectFn2 sendImpl input channel # toAffE
  case res of
    "ok" -> pure SendingOK
    "timed out" -> pure SendingTimedOut
    "rate limited" -> pure SendingRateLimited
    _ -> throwError $ error $ "Supabase.Realtime.send: invalid response: " <> res

foreign import onImpl :: forall f cbi. EffectFn4 RealtimeListenType f (EffectFn1 cbi Unit) Channel Unit

on :: forall f cbi. RealtimeListenType -> f -> (cbi -> Effect Unit) -> Channel -> Effect Unit
on lt filter callback channel = runEffectFn4 onImpl lt filter (mkEffectFn1 callback) channel

foreign import subscribeImpl
  :: EffectFn3
       (EffectFn2 String (UndefinedOr Error) Unit)
       Milliseconds
       Channel
       Unit

subscribe
  :: Milliseconds
  -> (RealtimeSubscribeState -> Maybe Error -> Effect Unit)
  -> Channel
  -> Effect Unit
subscribe timeout cb channel =
  runEffectFn3 subscribeImpl
    ( mkEffectFn2 \st err ->
        case SubscribeState.fromString st of
          Nothing ->
            cb ChannelError (Just (error $ "Supabase.Realtime.subscribe: invalid subscribe state: " <> st))
          Just ok ->
            cb ok (uorToMaybe err)
    )
    timeout
    channel
