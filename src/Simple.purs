module Simple where

import Data.Argonaut (decodeJson)
import Data.Argonaut.Decode.Class (class DecodeJson)
import Data.Argonaut.Parser (jsonParser)
import Data.Bifunctor (lmap)
import Data.Either (Either(Left))
import Network.HTTP.Affjax (AffjaxResponse)
import Network.HTTP.StatusCode (StatusCode(..))
import Prelude (otherwise, ($), (&&), (<), (<<<), (<=<), (==), (>=))

class MapStatusCode a where
  mapStatusCode  :: StatusCode -> a
  mapParserError :: String -> a

statusOk :: StatusCode -> Boolean
statusOk (StatusCode n) = n >= 200 && n < 300

decodeWithError :: forall a b.
                   MapStatusCode a
                => DecodeJson b
                => AffjaxResponse String
                -> Either a b
decodeWithError res
  | statusOk res.status = lmap mapParserError (decodeJson <=< jsonParser $ res.response)
  | otherwise           = Left $ mapStatusCode res.status
