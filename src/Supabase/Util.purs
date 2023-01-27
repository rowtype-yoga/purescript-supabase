module Supabase.Util
  ( fromEither
  , fromJSON
  , toError
  ) where

import Prelude
import Prim.Row (class Nub, class Union)

import Control.Monad.Error.Class (class MonadThrow)
import Data.Either (Either, either)
import Data.List.NonEmpty (NonEmptyList)
import Data.Semigroup.Foldable (intercalateMap)
import Effect.Aff (Error, error, throwError)
import Foreign (Foreign, ForeignError)
import Record (merge)
import Yoga.JSON as Yoga
import Yoga.JSON.Error (renderHumanError)

toError :: NonEmptyList ForeignError -> Error
toError = intercalateMap "\n" renderHumanError >>> error

fromJSON :: forall json m. MonadThrow Error m => Yoga.ReadForeign json ⇒ Foreign -> m json
fromJSON = Yoga.read >>> either (toError >>> throwError) pure

fromEither ∷ ∀ t m. MonadThrow Error m ⇒ Either Error t → m t
fromEither = either throwError pure
