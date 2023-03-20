module Supabase.Storage
  ( FileOptions
  , Storage
  , download
  , from
  , remove
  , storage
  , upload
  , createSignedUrl
  ) where

import Prelude

import Control.Promise (Promise)
import Control.Promise as Promise
import Data.Function.Uncurried (Fn3, runFn3)
import Data.Maybe (Maybe(..))
import Data.Nullable (Nullable)
import Data.Nullable as Nullable
import Effect (Effect)
import Effect.Aff (Aff)
import Foreign (Foreign)
import Supabase.Supabase (QueryBuilder, Response, ResultError)
import Supabase.Supabase as SBTypes
import Supabase.Types (Client)
import Type.Function (type ($))
import Supabase.Util as Util
import Web.File.Blob (Blob)
import Web.File.File as File
import Data.Time.Duration (Seconds)

foreign import data Storage :: Type

foreign import storage :: Client -> Storage
foreign import fromImpl :: Storage -> String -> QueryBuilder

from :: String -> Storage -> QueryBuilder
from = flip fromImpl

type FileOptions = { upsert :: Boolean }

foreign import uploadImpl :: QueryBuilder -> Fn3 String File.File FileOptions $ Effect (Promise Foreign)

upload ∷ String → File.File → FileOptions -> QueryBuilder → Aff (Response { path :: String })
upload filePath file fileOptions qb = runFn3 (uploadImpl qb) filePath file fileOptions # Promise.toAffE >>= Util.fromJSON

foreign import downloadImpl :: QueryBuilder -> String -> Effect (Promise { "data" :: Nullable Blob, error :: Nullable ResultError })

download :: String -> QueryBuilder -> Aff (SBTypes.Response Blob) -- [TODO] Make this a proper response
download file qb = downloadImpl qb file # Promise.toAffE <#> convert
  where
  convert { "data": d, error: error } = do
    let blob = Nullable.toMaybe d
    { "data": blob, error: Nullable.toMaybe error, status: Nothing }

foreign import removeImpl :: QueryBuilder -> Array String -> Effect (Promise Foreign)

remove ∷ Array String → QueryBuilder → Aff { error ∷ Maybe ResultError }
remove file qb = removeImpl qb file # Promise.toAffE >>= Util.fromJSON

foreign import createSignedUrlImpl :: QueryBuilder -> String -> Seconds -> Effect (Promise { "data" :: Nullable { signedUrl :: String }, error :: Nullable ResultError })

createSignedUrl :: String -> Seconds -> QueryBuilder -> Aff (SBTypes.Response { signedUrl :: String })
createSignedUrl file expiry qb = createSignedUrlImpl qb file expiry # Promise.toAffE <#> convert
  where
  convert { "data": d, error: error } = do
    let blob = Nullable.toMaybe d
    { "data": blob, error: Nullable.toMaybe error, status: Nothing }