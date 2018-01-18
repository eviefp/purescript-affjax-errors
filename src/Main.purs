module Main where

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
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

class MapStatusCode a where
  mapStatusCode  :: StatusCode -> a
  mapParserError :: String -> a

_unAuthorized = SProxy :: SProxy "unAuthorized"
_serverError  = SProxy :: SProxy "serverError"
_parseError   = SProxy :: SProxy "parseError"
_notFound     = SProxy :: SProxy "notFound"

parseError :: Variant (parseError :: Unit)
parseError = inj _parseError unit

mapBasicError :: StatusCode -> Variant (unAuthorized :: Unit, serverError :: Unit, parseError :: Unit)
mapBasicError (StatusCode n)
  | n == 401  = inj _unAuthorized unit
  | otherwise = inj _serverError  unit

mapNotFound :: StatusCode -> Variant (unAuthorized :: Unit, serverError :: Unit, notFound :: Unit, parseError :: Unit)
mapNotFound sc@(StatusCode n)
  | n == 404  = inj _notFound unit
  | otherwise = expand $ mapBasicError sc

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

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  log "Hello sailor!"
