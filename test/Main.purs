module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Class.Console (log)
import Supabase (Client)
import Supabase.Supabase as SB
import Unsafe.Coerce (unsafeCoerce)

main :: Effect Unit
main = do
  log "🍝"
  log "You should add some tests."
