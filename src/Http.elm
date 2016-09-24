module Http exposing
  (
  )

{-|

# Simple Requests
@docs get, post, Error

# URLs
@docs url, encodeUri, decodeUri

# Custom Requests
@docs Request, send, request, Method

# Parallel Requests
@docs race, parallel, parallel2, parallel3, parallel4, parallel5

# Chaining Requests
@docs map, andThen, onError, succeed, fail

# Request Bodies
@docs Body, emptyBody, stringBody, multipartBody, Part, stringPart

# Low-Level Stuff
@docs toTask, rawRequest

-}

import Json.Decode as Decode
import Json.Encode as Encode
import Native.Http
import Platform.Cmd as Cmd exposing (Cmd)
import Result exposing (Result)


-- SIMPLE REQUESTS


getString : String -> Cmd (Result Error String)


get : String -> Decode.Decoder a -> Cmd (Result Error a)


post : String -> Body -> Decode.Decoder a -> Cmd (Result Error a)


type Error



-- CUSTOM REQUESTS


type Request a = Request


send : (Error -> msg) -> (a -> msg) -> Request a -> Cmd msg


request
  : { method : Method
    , headers : List (String, String)
    , url : String
    , body : Body
    , decoder : Decode.Decoder a
    }
  -> Request a


type Method
  = GET
  | HEAD
  | POST
  | PUT
  | DELETE
  | CONNECT
  | OPTIONS
  | TRACE
  | PATCH
  | Method String



-- PARALLEL REQUESTS


race : Request a -> List (Request a) -> Request a


parallel : List (Request a) -> Request (List a)
parallel requests =
  List.foldr (parallel2 (::)) (succeed []) requests


parallel2 : (a -> b -> result) -> Request a -> Request b -> Request result


parallel3 : (a -> b -> c -> result) -> Request a -> Request b -> Request c -> Request result


parallel4 : (a -> b -> c -> d -> result) -> Request a -> Request b -> Request c -> Request d -> Request result


parallel5 : (a -> b -> c -> d -> e -> result) -> Request a -> Request b -> Request c -> Request d -> Request e -> Request result



-- CHAINING REQUESTS


map : (a -> b) -> Request a -> Request b


andThen : (a -> Request b) -> Request a -> Request b


onError : (Error -> Request b) -> Request a -> Request b


succeed : a -> Request a


fail : String -> Request a



-- REQUEST BODIES


type Body


emptyBody : Body


stringBody : String -> Body


multipartBody : List Part -> Body


type Part


stringPart : String -> Part



-- LOW LEVEL


toTask : Request a -> Task Error a


rawRequest : ? -> Request Response
