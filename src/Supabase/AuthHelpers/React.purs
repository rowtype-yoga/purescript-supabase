module Supabase.AuthHelpers.React where

import Prelude

import Data.Maybe (Maybe)
import Data.Nullable (Nullable)
import Data.Nullable as Nullable
import Effect (Effect)
import Foreign (Foreign)
import React.Basic.Hooks (Hook, unsafeHook)
import Record.Studio (mapRecordKind)
import Supabase.Supabase as Supabase
import Supabase.Types (Client)
import Yoga.JSON as YogaJSON
import Yoga.JSON (class WriteForeign)

foreign import useUserImpl :: Effect { user :: Nullable Supabase.User }

useUser :: Hook (UseUser) { user :: Maybe Supabase.User }
useUser = unsafeHook useUserImpl <#> mapRecordKind Nullable.toMaybe

foreign import data UseUser :: Type -> Type
