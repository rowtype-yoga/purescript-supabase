module Supabase.AuthHelpers
  ( UseUser
  , createBrowserClient
  , useUser
  , userProvider
  )
  where

import Prelude

import Data.Maybe (Maybe)
import Data.Nullable (Nullable)
import Data.Nullable as Nullable
import Effect (Effect)
import React.Basic.Hooks (Hook, ReactComponent, unsafeHook)
import Record.Studio (mapRecordKind)
import Supabase.Supabase as Supabase
import Supabase.Types (Client)

foreign import userProvider :: forall props. ReactComponent { supabaseClient :: Client | props }

foreign import useUserImpl :: Effect { user :: Nullable Supabase.User }

useUser :: Hook (UseUser) { user :: Maybe Supabase.User }
useUser = unsafeHook useUserImpl <#> mapRecordKind Nullable.toMaybe

foreign import data UseUser :: Type -> Type

foreign import createBrowserClient :: Effect Client
