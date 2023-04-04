module Supabase.Realtime.ListenType where

import Prelude
import Unsafe.Coerce (unsafeCoerce)
import Unsafe.Reference (unsafeRefEq)

foreign import data RealtimeListenType :: Type
instance Eq RealtimeListenType where
  eq = unsafeRefEq

broadcast :: RealtimeListenType
broadcast = unsafeCoerce "broadcast"

presence :: RealtimeListenType
presence = unsafeCoerce "presence"

postgresChanges :: RealtimeListenType
postgresChanges = unsafeCoerce "postgres_changes"
