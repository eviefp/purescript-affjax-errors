module Variant where

import Data.Argonaut (decodeJson)
import Data.Argonaut.Decode.Class (class DecodeJson)
import Data.Argonaut.Parser (jsonParser)
import Data.Bifunctor (lmap)
import Data.Either (Either(Left))
import Data.Symbol (SProxy(..))
import Data.Variant (Variant, expand, inj)
import Network.HTTP.Affjax (AffjaxResponse)
import Network.HTTP.StatusCode (StatusCode(..))
import Prelude (Unit, otherwise, unit, ($), (&&), (<), (<<<), (<=<), (==), (>=))



statusOk :: StatusCode -> Boolean
statusOk (StatusCode n) = n >= 200 && n < 300

-- | We can't use (StatusCode -> Variant i) because reasons.
-- | Feels like it's wrong that we can't, but the non-Variant version suffers from the same problem.
-- | ... although, it's not as explicitly layed out as here. You don't ostentatively throw a constructor
-- | you're never going to use in the type declaration of the map function.
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
