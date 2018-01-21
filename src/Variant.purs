module Variant
  ( decodeWithError
  , runAffjaxWithError
  ) where

import Control.Monad.Aff (Aff)
import Data.Argonaut (decodeJson)
import Data.Argonaut.Decode.Class (class DecodeJson)
import Data.Argonaut.Parser (jsonParser)
import Data.Bifunctor (lmap)
import Data.Either (Either(Left))
import Data.Variant (Variant, expand)
import Network.HTTP.Affjax (AJAX, AffjaxRequest, AffjaxResponse, affjax)
import Network.HTTP.Affjax.Request (class Requestable)
import Network.HTTP.StatusCode (StatusCode(..))
import Prelude (otherwise, ($), (&&), (<), (<$>), (<<<), (<=<), (>=))

statusOk :: StatusCode -> Boolean
statusOk (StatusCode n) = n >= 200 && n < 300

-- | We need ways to map both possible errors: status code and parse errors. Each of these map
-- | to a diferent Variant type, so we also need to make sure the resulting Variant does
-- | not have duplicates, which is why we need two `Union` constraints.
decodeWithError :: forall a i p o.
                   DecodeJson a
                => Union i p o
                => Union p i o
                => (StatusCode -> Variant i)
                -> (String -> Variant p)
                -> AffjaxResponse String
                -> Either (Variant o) a
decodeWithError errorMapper peMapper response
  | statusOk response.status = lmap (expand <<< peMapper) (decodeJson <=< jsonParser $ response.response)
  | otherwise                = Left <<< expand <<< errorMapper $ response.status

runAffjaxWithError
  ∷ ∀ a b i p o eff
  . Requestable a
  ⇒ DecodeJson b
  ⇒ Union i p o
  ⇒ Union p i o
  ⇒ (StatusCode → Variant i)
  → (String → Variant p)
  → AffjaxRequest a
  → Aff (ajax ∷ AJAX | eff) (Either (Variant o) b)
runAffjaxWithError statusCodeMap parseErrorMap req =
  decodeWithError statusCodeMap parseErrorMap <$> affjax req
