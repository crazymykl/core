module Json.Decode exposing
  ( Decoder, string, int, float, bool
  , object, required, optional, hardcoded, custom
  , nullable, list, array, dict, keyValuePairs
  , oneOf
  , decodeString, decodeValue, Value
  , map, map2, map3, map4, map5
  , field, index, null, value
  , lazy, andThen, succeed, fail
  )

{-| Turn JSON values into Elm values. Definitely check out this [intro to JSON
decoders][guide] to get a feel for how this library works!

[guide]: https://guide.elm-lang.org/interop/json.html

# Primitives
@docs Decoder, string, int, float, bool

# Objects
@docs object, required, optional, hardcoded, custom

# Data Structures
@docs nullable, list, array, dict, keyValuePairs

# Versioning
@docs oneOf

# Run Decoders
@docs decodeString, decodeValue, Value

# Mapping
@docs map, map2, map3, map4, map5

# Fancy Primitives
@docs field, index, null, value

# Fancy Decoding
@docs lazy, andThen, succeed, fail

-}


import Array exposing (Array)
import Basics exposing ((<|), (|>))
import Dict exposing (Dict)
import Json.Encode as JsEncode
import List
import Maybe exposing ( Maybe(..) )
import Result exposing ( Result(..) )
import Native.Json



-- PRIMITIVES


{-| A value that knows how to decode JSON values.
-}
type Decoder a = Decoder


string : Decoder String
string =
  Native.Json.decodePrimitive "string"


int : Decoder Int
int =
  Native.Json.decodePrimitive "int"


float : Decoder Float
float =
  Native.Json.decodePrimitive "float"


bool : Decoder Bool
bool =
  Native.Json.decodePrimitive "bool"



-- OBJECTS


apply : (a -> b) -> a -> b
apply f v =
  f v


object : a -> Decoder a
object =
  succeed


required : String -> Decoder a -> Decoder (a -> b) -> Decoder b
required name decoder funcDecoder =
  map2 apply funcDecoder <|
    field name decoder


optional : String -> Decoder a -> a -> Decoder (a -> b) -> Decoder b
optional name decoder fallback funcDecoder =
  map2 apply funcDecoder <|
    oneOf
      [ field name decoder
      , succeed fallback
      ]


hardcoded : a -> Decoder (a -> b) -> Decoder b
hardcoded value funcDecoder =
  map2 apply funcDecoder (succeed value)


custom : Decoder a -> Decoder (a -> b) -> Decoder b
custom valueDecoder funcDecoder =
  map2 apply funcDecoder valueDecoder



-- DATA STRUCTURES


nullable : Decoder a -> Decoder (Maybe a)
nullable decoder =
  oneOf
    [ null Nothing
    , map Just decoder
    ]


list : Decoder a -> Decoder (List a)
list decoder =
  Native.Json.decodeContainer "list" decoder


array : Decoder a -> Decoder (Array a)
array decoder =
  Native.Json.decodeContainer "array" decoder


dict : Decoder a -> Decoder (Dict String a)
dict decoder =
  map Dict.fromList (keyValuePairs decoder)


keyValuePairs : Decoder a -> Decoder (List (String, a))
keyValuePairs =
  Native.Json.decodeKeyValuePairs




-- VERSIONING


oneOf : List (Decoder a) -> Decoder a
oneOf =
    Native.Json.oneOf


-- RUN DECODERS


decodeString : Decoder a -> String -> Result String a
decodeString =
  Native.Json.runOnString


{-| A JSON value.
-}
type alias Value = JsEncode.Value


decodeValue : Decoder a -> Value -> Result String a
decodeValue =
  Native.Json.run



-- MAPS


{-| Transform a decoder. In the following example, we use `map` and `oneOf`
to handle user names that may be in the &ldquo;old&rdquo; or
&ldquo;new&rdquo; format.

    type UserName = Old Int | New String

    -- 1234 or "1234abc"
    userName : Decoder UserName
    userName =
        oneOf
          [ map Old int
          , map New string
          ]

We also use `map` and `oneOf` in defining `nullable`:

    nullable : Decoder a -> Decoder (Maybe a)
    nullable decoder =
        oneOf
          [ null Nothing
          , map Just decoder
          ]
-}
map : (a -> b) -> Decoder a -> Decoder b
map =
  Native.Json.map


map2 : (a -> b -> value) -> Decoder a -> Decoder b -> Decoder value
map2 =
    Native.Json.map2


{-|-}
map3 : (a -> b -> c -> value) -> Decoder a -> Decoder b -> Decoder c -> Decoder value
map3 =
    Native.Json.map3


{-|-}
map4 : (a -> b -> c -> d -> value) -> Decoder a -> Decoder b -> Decoder c -> Decoder d -> Decoder value
map4 =
    Native.Json.map4


{-|-}
map5 : (a -> b -> c -> d -> e -> value) -> Decoder a -> Decoder b -> Decoder c -> Decoder d -> Decoder e -> Decoder value
map5 =
    Native.Json.map5



-- FANCY PRIMITIVES


field : String -> Decoder a -> Decoder a
field =
  Native.Json.decodeField


index : Int -> Decoder a -> Decoder a
index =
  Native.Json.decodeIndex


null : a -> Decoder a
null =
  Native.Json.decodeNull


value : Decoder Value
value =
  Native.Json.decodePrimitive "value"



-- FANCY DECODERS


lazy : (() -> Decoder a) -> Decoder a
lazy thunk =
  let
    lazilyDecode jsValue =
      case decodeValue (thunk ()) jsValue of
        Ok value ->
          succeed value

        Err msg ->
          fail msg
  in
    andThen lazilyDecode value


andThen : (a -> Decoder b) -> Decoder a -> Decoder b
andThen =
  Native.Json.andThen


succeed : a -> Decoder a
succeed =
  Native.Json.succeed


fail : String -> Decoder a
fail =
  Native.Json.fail

