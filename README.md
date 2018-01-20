# Purescript Affjax Errors
Simple Affjax requests that allow for HTTP errors.

## Motivation
> [Affjax](https://pursuit.purescript.org/packages/purescript-affjax) is a library taking advantage
> of `Aff` to enable pain-free asynchronous AJAX requests and response handling.

Although the library is very easy to use, it suffers from one major drawback: it cannot be fully
used in the presence of HTTP errors. I will go over why in the following section.

Please note that the maintainers of `Affjax` are already planning a major update that will fix this
problem. At the time of writing this document, the version is `5.0.0`.

## Problem with Affjax
At the core of the library, we have the following function:

```purescript
type Affjax e a = Aff (ajax :: AJAX | e) (AffjaxResponse a)

affjax
  :: forall e a b
   . Requestable a
  => Respondable b
  => AffjaxRequest a
  -> Affjax e b
```

As long as we stay on the happy path, this is all trivially easy to use. We create
`AffjaxRequest` and `DencodeJson` instances for whatever it is we're expecting from the API, and we
get back the result.

However, regardless of the status code returned by the request, `affjax` will attempt to parse the
content and will fail. See the implementation of
[affjax](https://github.com/slamdata/purescript-affjax/blob/v5.0.0/src/Network/HTTP/Affjax.purs#L230),
specifically its `fromResponse'` helper.

## My Solution
We could bypass this entire problem by not asking `affjax` to do any Json decoding. If we ask it for
a plain `String`, then we can check for the `StatusCode` (which is an `Int` newtype)ourselves and decide
whether we return an error or if we try to parse the string through Json to a well-formed type.

The problem is then moved to, how do we best express HTTP errors. This repository presents two
alternatives: a **simple** version which uses sum types and a slightly more elaborate version
dubbed **variant** which uses the
[purescript-variant](https://pursuit.purescript.org/packages/purescript-variant) library.

## Simple implementation
We start with the observation that we need a way of transforming from a `StatusCode` to an error
sum type:

```purescript
class MapStatusCode a where
  mapStatusCode  :: StatusCode -> a
  mapParserError :: String -> a
```

`MapStatusCode` allows us to map a StatusCode to any type with an instance, as well as map parsing
errors (whose errors are expressed as String in
[purescript-argonaut](https://pursuit.purescript.org/packages/purescript-argonaut).

Having all of this, we can write a generic `AffjaxResponse String` to `Either errorType resultType`
function:

```purescript
decodeWithError :: forall a b.
                   MapStatusCode a
                => DecodeJson b
                => AffjaxResponse String
                -> Either a b
decodeWithError res
  | statusOk res.status = lmap mapParserError (decodeJson <=< jsonParser $ res.response)
  | otherwise           = Left $ mapStatusCode res.status
```

The implementation is simple: if the status is ok (in the [200, 300) range), we attempt to parse
it as json. Otherwise, we map the status code to the error type.

Using this method is equally simple. We need to define an error type and its `MapStatusCode`
instance:

```purescript
data BasicError = Unauthorized | ServerError | ParseError

instance basicErrorMapStatusCode :: MapStatusCode BasicError where
  mapStatusCode (StatusCode n)
    | n == 401  = Unauthorized
    | otherwise = ServerError
  mapParserError _ = ParseError
```

And then we can implement an API function that uses `affjax`:

```purescript
getFile :: forall eff. String -> Aff (ajax :: AJAX | eff) (Either BasicError String)
getFile s = do
  res <- affjax $ defaultRequest
    { url = "simpleAPI/" <> s
    , method = Left GET
    }
  pure $ S.decodeWithError res
```

The only real problem with this approach is building on top of it. If we need "something like
BasicError, but with NotFound", we need to create an entirely new type:

```purescript
data SomeError = NotFound | SomeBasicError BasicError

instance someErrorMapStatusCode :: MapStatusCode SomeError where
  mapStatusCode sc@(StatusCode n)
    | n == 404  = NotFound
    | otherwise = SomeBasicError $ mapStatusCode sc
  mapParserError = SomeBasicError <<< mapParserError
```

We can reuse `BasicError`s `MapStatusCode` instance, but we need to wrap it into a constructor.
An additional problem is when we need to match the error, we end up with something like:

```purescript
res <- getFilePlus "data.json"
let str = case res of
            Left err -> case err of
              NotFound -> "not found"
              SomeBasicError Unauthorized -> "unauthorized"
              SomeBasicError ServerError -> "server error"
              SomeBasicError ParseError -> "parse error"
            Right x -> x
```

If that doesn't look bad, imagine having to add a `490 Conflict` on top of `NotFound`.

## Variant implementation
If you are not already familiar with the excellent 
[purescript-variant](https://pursuit.purescript.org/packages/purescript-variant) library, please
go check it out. It has an excellent readme.

If we use `Variant` for our error type, then our `decodeWithError` function becomes:

```purescript
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
```

Our function now takes two additional parameters, which map to the class we ditched. Basically, we
need something to map fail status codes to a Variant type, and something to map parser errors to
a different Variant type. This is an important note, the two Variants need to have no row in common,
which is expressed by the double `Union` constraint.

We can now create a new `BasicError'` type as a `Variant`, and write out the two required functions:
`mapBasicError` and `parseError`:

```purescript
_unAuthorized = SProxy :: SProxy "unAuthorized"
_serverError  = SProxy :: SProxy "serverError"
_parseError   = SProxy :: SProxy "parseError"

type BasicError' =
  Variant
    ( unAuthorized :: Unit
    , serverError  :: Unit
    , parseError   :: Unit
    )

mapBasicError :: StatusCode -> Variant (unAuthorized :: Unit, serverError :: Unit)
mapBasicError (StatusCode n)
  | n == 401  = inj _unAuthorized unit
  | otherwise = inj _serverError  unit

parseError :: Variant (parseError :: Unit)
parseError = inj _parseError unit
```

We can now create the same `getFile'` API method, but using the `Variant` alternative:

```purescript
getFile' :: forall eff. String -> Aff (ajax :: AJAX | eff) (Either BasicError' String)
getFile' s = do
  res <- affjax $ defaultRequest
    { url = "simpleAPI/" <> s
    , method = Left GET
    }
  pure $ decodeWithError mapBasicError (const parseError) res
```

As before, adding the `NotFound` row is trivial:

```purescript
_notFound     = SProxy :: SProxy "notFound"

type SomeError' =
  Variant
    ( unAuthorized :: Unit
    , serverError  :: Unit
    , notFound     :: Unit
    , parseError   :: Unit
    )

mapNotFound :: StatusCode -> Variant (unAuthorized :: Unit, serverError :: Unit, notFound :: Unit)
mapNotFound sc@(StatusCode n)
  | n == 404  = inj _notFound unit
  | otherwise = expand $ mapBasicError sc

getFilePlus' :: forall eff. String -> Aff (ajax :: AJAX | eff) (Either SomeError' String)
getFilePlus' s = do
  res <- affjax $ defaultRequest
    { url = s
    , method = Left GET
    }
  pure $ decodeWithError mapNotFound (const parseError) res
```

And the best part, matching the error is not layered:

```purescript
res' <- getFilePlus' "data.json"
let str' = case res' of
            Left err ->
              case_
                # on _notFound (const "not found")
                # on _unAuthorized (const "unauthorized")
                # on _serverError (const "server error")
                # on _parseError (const "parse error")
                $ err
            Right x -> x
```
