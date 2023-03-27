module Supabase.Supabase
  ( AuthResponse
  , Count(..)
  , CountR
  , CountResponse
  , DataR
  , ErrorR
  , FilterBuilder
  , FunctionResponse
  , QueryBuilder
  , Response
  , ResultError
  , Session
  , SessionData
  , StatusR
  , User
  , class Equals
  , class Select
  , delete
  , equals
  , from
  , getSession
  , invoke
  , onAuthStateChange
  , range
  , select
  , signInWithOtp
  , signOut
  , single
  , maybeSingle
  , update
  , upsert
  ) where

import Prelude

import Control.Promise (Promise)
import Control.Promise as Promise
import Data.Function.Uncurried (Fn2, Fn3, runFn2, runFn3)
import Data.Maybe (Maybe)
import Data.Nullable (Nullable)
import Data.Nullable as Nullable
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff, Error)
import Effect.Uncurried (EffectFn1, mkEffectFn1)
import Fetch as Fetch
import Fetch.Core.Response as YogaJson.Core
import Fetch.Internal.Response as FetchInternalResponse
import Foreign (Foreign)
import Supabase.Types (Client)
import Type.Function (type ($))
import Type.Row (type (+))
import Supabase.Util as Util
import Yoga.JSON (class ReadForeign, class WriteForeign, write) as YogaJson
import Yoga.JSON (class ReadForeign, class WriteForeign, writeImpl)

foreign import data QueryBuilder :: Type
foreign import data FilterBuilder :: Type

data Count = Exact | Planned | Estimated

countToString ∷ Count → String
countToString Exact = "exact"
countToString Planned = "planned"
countToString Estimated = "estimated"

foreign import deleteImpl :: QueryBuilder -> FilterBuilder

delete ∷ QueryBuilder -> FilterBuilder
delete = deleteImpl

foreign import updateImpl :: QueryBuilder -> Foreign -> FilterBuilder

update ∷ forall d. YogaJson.WriteForeign d => d -> QueryBuilder -> FilterBuilder
update d builder = updateImpl builder $ YogaJson.write d

foreign import fromImpl :: Client -> String -> QueryBuilder

from ∷ String → Client -> QueryBuilder
from s c = fromImpl c s

foreign import upsertImpl :: QueryBuilder -> Foreign -> FilterBuilder

upsert ∷ ∀ (values ∷ Type). WriteForeign values => values → QueryBuilder → FilterBuilder
upsert v q = upsertImpl q $ writeImpl v

foreign import selectRunImpl :: forall input. FilterBuilder -> input -> Effect (Promise Foreign)

foreign import selectQueryImpl :: QueryBuilder -> String -> FilterBuilder

foreign import selectQueryWithCountImpl :: QueryBuilder -> String -> String -> FilterBuilder

class Select input builder output | output -> builder where
  select :: input -> builder -> output

instance (YogaJson.ReadForeign output) => Select Unit FilterBuilder (Aff output) where
  select input builder = selectRunImpl builder (input) # Promise.toAffE >>= Util.fromJSON
else instance (YogaJson.ReadForeign output) => Select String FilterBuilder (Aff output) where
  select input builder = selectRunImpl builder input # Promise.toAffE >>= Util.fromJSON
else instance Select (Tuple String Count) QueryBuilder FilterBuilder where
  select (Tuple input count) builder = selectQueryWithCountImpl builder input $ countToString count
else instance Select String QueryBuilder FilterBuilder where
  select = flip selectQueryImpl

foreign import eqRunImpl :: FilterBuilder -> Fn2 String String $ Effect (Promise Foreign)

foreign import eqImpl :: forall builderIn builderOut. builderIn -> Fn2 String String builderOut

class Equals builder output | output -> builder where
  equals :: String -> String -> builder -> output

instance Equals FilterBuilder FilterBuilder where
  equals :: String -> String -> FilterBuilder -> FilterBuilder
  equals k v fb = runFn2 (eqImpl fb) k v
else instance Equals QueryBuilder FilterBuilder where
  equals :: String -> String -> QueryBuilder -> FilterBuilder
  equals k v fb = runFn2 (eqImpl fb) k v
else instance (YogaJson.ReadForeign output) => Equals FilterBuilder (Aff output) where
  equals :: String -> String -> FilterBuilder -> Aff output
  equals l r builder = runFn2 (eqRunImpl builder) l r # Promise.toAffE >>= Util.fromJSON

foreign import singleImpl :: FilterBuilder -> Effect (Promise Foreign)

single :: forall t. YogaJson.ReadForeign t => FilterBuilder -> Aff (Response t)
single = singleImpl >>> Promise.toAffE >=> Util.fromJSON

foreign import maybeSingleImpl :: FilterBuilder -> Effect (Promise Foreign)

maybeSingle :: forall t. YogaJson.ReadForeign t => FilterBuilder -> Aff (Response t)
maybeSingle = maybeSingleImpl >>> Promise.toAffE >=> Util.fromJSON

foreign import rangeImpl :: forall i. i -> i -> FilterBuilder -> Effect (Promise Foreign)

type CountResponse t = { | DataR t + ErrorR + CountR + StatusR + () }

range :: forall response. YogaJson.ReadForeign response => { from :: Int, to :: Int } -> FilterBuilder -> Aff response
range { from: f, to } = rangeImpl f to >>> Promise.toAffE >=> Util.fromJSON

type InternalAuthResponse = { error :: Nullable Error }

foreign import signInWithOtpImpl :: Client -> String -> Effect (Promise InternalAuthResponse)

type AuthResponse = { error :: Maybe Error }

signInWithOtp :: Client -> String -> Aff AuthResponse
signInWithOtp client email = signInWithOtpImpl client email # Promise.toAffE <#> \{ error } -> { error: Nullable.toMaybe error }

foreign import signOutImpl :: Client -> Effect (Promise Unit)

signOut ∷ Client → Aff Unit
signOut client = signOutImpl client # Promise.toAffE

foreign import onAuthStateChangeImpl :: Client -> (EffectFn1 (Nullable Session) Unit) -> Effect { data :: { id :: String, unsubscribe :: Effect Unit } }

onAuthStateChange ∷ Client → (Maybe Session → Effect Unit) → Effect { data :: { id :: String, unsubscribe :: Effect Unit } }
onAuthStateChange client handler = onAuthStateChangeImpl client $ mkEffectFn1 (Nullable.toMaybe >>> handler)

type User = { id :: String, email :: String }
type Session = { user :: User }
type SessionData = { session :: Maybe Session }

type ResultError = { code :: Maybe String, details :: Maybe String, message :: String }

type DataR d r = (data :: Maybe d | r)
type ErrorR r = (error ∷ Maybe ResultError | r)
type CountR r = (count :: Int | r)

type StatusR :: forall k. k -> Row Type
type StatusR r = (status ∷ Maybe Int)

type Response d = { data :: Maybe d, error ∷ Maybe ResultError, status ∷ Maybe Int }

foreign import getSessionImpl :: Client -> Effect (Promise Foreign)

getSession :: Client -> Aff (Response SessionData)
getSession client = getSessionImpl client # Promise.toAffE >>= Util.fromJSON

type InternalFunctionResponse d = { "data" :: Nullable d, error :: Nullable { message :: String, context :: YogaJson.Core.Response } }

foreign import invokeImpl :: forall body headers t. Client -> Fn3 String body headers (Effect $ Promise $ InternalFunctionResponse t)

type FunctionResponse d = { "data" :: Maybe d, error :: Maybe { message :: String, context :: Fetch.Response } }

invoke ∷ forall t body headers. ReadForeign t ⇒ Client → String → body → headers → Aff (FunctionResponse t)
invoke client fn body headers = runFn3 (invokeImpl client) fn body headers # Promise.toAffE <#> convert
  where
  convertError { message, context } = { message, context: FetchInternalResponse.convert context }
  convert { "data": d, error } = { "data": Nullable.toMaybe d, error: Nullable.toMaybe error <#> convertError }
