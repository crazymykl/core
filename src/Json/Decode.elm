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


{-| Decode a JSON string into an Elm `String`.

    decodeString string "true"              == Err ...
    decodeString string "42"                == Err ...
    decodeString string "3.14"              == Err ...
    decodeString string "\"hello\""         == Ok "hello"
    decodeString string "{ \"hello\": 42 }" == Err ...
-}
string : Decoder String
string =
  Native.Json.decodePrimitive "string"


{-| Decode a JSON number into an Elm `Int`.

    decodeString int "true"              == Err ...
    decodeString int "42"                == Ok 42
    decodeString int "3.14"              == Err ...
    decodeString int "\"hello\""         == Err ...
    decodeString int "{ \"hello\": 42 }" == Err ...
-}
int : Decoder Int
int =
  Native.Json.decodePrimitive "int"


{-| Decode a JSON number into an Elm `Float`.

    decodeString float "true"              == Err ..
    decodeString float "42"                == Ok 42
    decodeString float "3.14"              == Ok 3.14
    decodeString float "\"hello\""         == Err ...
    decodeString float "{ \"hello\": 42 }" == Err ...
-}
float : Decoder Float
float =
  Native.Json.decodePrimitive "float"


{-| Decode a JSON boolean into an Elm `Bool`.

    decodeString bool "true"              == Ok True
    decodeString bool "42"                == Err ...
    decodeString bool "3.14"              == Err ...
    decodeString bool "\"hello\""         == Err ...
    decodeString bool "{ \"hello\": 42 }" == Err ...
-}
bool : Decoder Bool
bool =
  Native.Json.decodePrimitive "bool"



-- OBJECTS


apply : (a -> b) -> a -> b
apply f v =
  f v


{-| A helper for decoding objects. You use it with functions like `required`
and `optional`. The guide to Elm also has [a whole section][guide] on using
JSON decoders, so check that out for more context.

[guide]: http://guide.elm-lang.org/reuse/json.html

**Note:** `object` is an alias for `succeed` that reads a bit nicer in
practice.
-}
object : a -> Decoder a
object =
  succeed


{-| Access a required field. For example, decoding a `Student` object would
look like this:

    type alias Student =
      { name : String
      , grade : Int
      }

    student : Decoder Student
    student =
      object Student
        |> required "name" string
        |> required "grade" int

    -- decodeString student "{ \"name\": \"John\", \"grade\": 4 }"
    --   == Ok (Student "John" 4)
    --
    -- decodeString student "{ \"name\": \"John\" }"
    --   == Err ...
    --
    -- decodeString student "{ \"grade\": 4 }"
    --   == Err ...
-}
required : String -> Decoder a -> Decoder (a -> b) -> Decoder b
required name decoder funcDecoder =
  map2 apply funcDecoder <|
    field name decoder


{-| Access an optional field. So if our JSON student objects may not have a
`grade` field in some cases, we could do the following:

    type alias Student =
      { name : String
      , grade : Maybe Int
      }

    student : Decoder Student
    student =
      object Student
        |> required "name" string
        |> optional "grade" (nullable int) Nothing

    -- decodeString student "{ \"name\": \"John\", \"grade\": 4 }"
    --   == Ok (Student "John" (Just 4))
    --
    -- decodeString student "{ \"name\": \"John\", \"grade\": null }"
    --   == Ok (Student "John" Nothing)
    --
    -- decodeString student "{ \"name\": \"John\" }"
    --   == Ok (Student "John" Nothing)
    --
    -- decodeString student "{ \"grade\": 4 }"
    --   == Err ...
-}
optional : String -> Decoder a -> a -> Decoder (a -> b) -> Decoder b
optional name decoder fallback funcDecoder =
  map2 apply funcDecoder <|
    oneOf
      [ field name decoder
      , succeed fallback
      ]


{-| Sometimes you want a hardcoded value when constructing a record. For
example, maybe you know all students are in grade 7.

    type alias Student =
      { name : String
      , grade : Int
      }

    student : Decoder Student
    student =
      object Student
        |> required "name" string
        |> hardcoded 7

    -- decodeString student "{ \"name\": \"John\" }"
    --   == Ok (Student "John" 7)
    --
    -- decodeString student "{ \"name\": \"John\", \"grade\": 4 }"
    --   == Ok (Student "John" 7)
    --
    -- decodeString student "{ \"grade\": 4 }"
    --   == Err ...
-}
hardcoded : a -> Decoder (a -> b) -> Decoder b
hardcoded value funcDecoder =
  map2 apply funcDecoder (succeed value)


{-| The `custom` function is a safety valve for when your data is to crazy
for stuff like `required` and `optional`.
-}
custom : Decoder a -> Decoder (a -> b) -> Decoder b
custom valueDecoder funcDecoder =
  map2 apply funcDecoder valueDecoder



-- DATA STRUCTURES


{-| Decode a nullable JSON value into an Elm value.

    decodeString (nullable int) "13"    == Ok (Just 13)
    decodeString (nullable int) "42"    == Ok (Just 42)
    decodeString (nullable int) "null"  == Ok Nothing
    decodeString (nullable int) "true"  == Err ..
-}
nullable : Decoder a -> Decoder (Maybe a)
nullable decoder =
  oneOf
    [ null Nothing
    , map Just decoder
    ]


{-| Decode a JSON array into an Elm `List`.

    decodeString (list int) "[1,2,3]"       == Ok [1,2,3]
    decodeString (list bool) "[true,false]" == Ok [True,False]
-}
list : Decoder a -> Decoder (List a)
list decoder =
  Native.Json.decodeContainer "list" decoder


{-| Decode a JSON array into an Elm `Array`.

    decodeString (array int) "[1,2,3]"       == Ok (Array.fromList [1,2,3])
    decodeString (array bool) "[true,false]" == Ok (Array.fromList [True,False])
-}
array : Decoder a -> Decoder (Array a)
array decoder =
  Native.Json.decodeContainer "array" decoder


{-| Decode a JSON object into an Elm `Dict`.

    decodeString (dict int) "{ \"alice\": 42, \"bob\": 99 }"
      == Dict.fromList [("alice", 42), ("bob", 99)]
-}
dict : Decoder a -> Decoder (Dict String a)
dict decoder =
  map Dict.fromList (keyValuePairs decoder)


{-| Decode a JSON object into an Elm `List` of pairs.

    decodeString (keyValuePairs int) "{ \"alice\": 42, \"bob\": 99 }"
      == [("alice", 42), ("bob", 99)]
-}
keyValuePairs : Decoder a -> Decoder (List (String, a))
keyValuePairs =
  Native.Json.decodeKeyValuePairs




-- VERSIONING


{-| Try a bunch of different decoders. This can be useful if the JSON may come
in a couple different formats. For example, say you want to read an array of
numbers, but some of them are `null`.

    import String

    badInt : Decoder Int
    badInt =
      oneOf [ int, null 0 ]

    -- decodeString (list badInt) "[1,2,null,4]" == Ok [1,2,0,4]

Why would someone generate JSON like this? Questions like this are not good
for your health. The point is that you can use `oneOf` to handle situations
like this!

You could also use `oneOf` to help version your data. Try the latest format,
then a few older ones that you still support. You could use `andThen` to be
even more particular if you wanted.
-}
oneOf : List (Decoder a) -> Decoder a
oneOf =
    Native.Json.oneOf


-- RUN DECODERS


{-| Parse the given string into a JSON value and then run the `Decoder` on it.
This will fail if the string is not well-formed JSON or if the `Decoder`
fails for some reason.

    decodeString int "4"     == Ok 4
    decodeString int "1 + 2" == Err ...
-}
decodeString : Decoder a -> String -> Result String a
decodeString =
  Native.Json.runOnString


{-| A JSON value.
-}
type alias Value = JsEncode.Value


{-| Run a `Decoder` on some JSON `Value`. You can send these JSON values
through ports, so that is probably the main time you would use this function.
-}
decodeValue : Decoder a -> Value -> Result String a
decodeValue =
  Native.Json.run



-- MAPS


{-| Transform a decoder. Maybe you just want to know the length of a string:

    import String

    stringLength : Decoder Int
    stringLength =
      map String.length string

It is often helpful to use `map` with `oneOf`, like when defining `nullable`:

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


{-| Try two decoders and then combine the result. So normally we would decode
an `{x,y}` object like this:

    point : Decoder (Int, Int)
    point =
      object (,)
        |> required "x" int
        |> required "y" int

But we can use the more primitive `map2` function if we want:

    point : Decoder (Int, Int)
    point =
      map2 (,) (field "x" int) (field "y" int)

It tries each individual decoder and puts the result together. In this case,
a JSON object can be decoded in multiple ways depending on what fields you ask
for. Lots of functions in this library are defined in terms of `map2`.
-}
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


{-| Decode some field of a JSON object into an Elm value.

    decodeString (field "x" int)  "{ \"x\": 3, \"y\": 4 }" == Ok 3
    decodeString (field "y" int)  "{ \"x\": 3, \"y\": 4 }" == Ok 4
    decodeString (field "z" int)  "{ \"x\": 3, \"y\": 4 }" == Err ...
    decodeString (field "y" bool) "{ \"x\": 3, \"y\": 4 }" == Err ...

This is used to define `required` and `optional` behind the scenes.
-}
field : String -> Decoder a -> Decoder a
field =
  Native.Json.decodeField


{-| Decode some index of a JSON array into an Elm value.

    decodeString (index 0 int) [42,true] == Ok 42
    decodeString (index 1 int) [42,true] == Err ..
    decodeString (index 1 bool) [42,true] == Ok True
-}
index : Int -> Decoder a -> Decoder a
index =
  Native.Json.decodeIndex


{-| Decode a `null` value into some Elm value.

    decodeString (null False) "null" == Ok False
    decodeString (null 42) "null"    == Ok 42
    decodeString (null 42) "42"      == Err ..
    decodeString (null 42) "false"   == Err ..

So if you ever see a `null`, this will return whatever value you specified.
-}
null : a -> Decoder a
null =
  Native.Json.decodeNull


{-| Do not do anything with a JSON value, just bring it into Elm as a `Value`.
This can be useful if you have particularly crazy data that you would like to
deal with later. Or if you are going to send it out a port and do not care
about its structure.
-}
value : Decoder Value
value =
  Native.Json.decodePrimitive "value"



-- FANCY DECODERS


{-| Sometimes you have JSON with recursive structure, like comments that may
have *more* comments. You can use `lazy` to make sure your decoder unrolls
lazily.

    type alias Comment =
      { message : String
      , responses : Responses
      }

    type Responses = Responses (List Comment)

    comment : Decoder Comment
    comment =
      object Comment
        |> required "message" string
        |> required "responses" (map Responses (list (lazy (\_ -> comment))))

If we had said `list comment` instead, we would start expanding the value
infinitely. What is a `comment`? It is a decoder for objects where the
`responses` field contains comments. What is a `comment` though? Etc.

By using `list (lazy (\_ -> comment))` we make sure the decoder only expands
to be as deep as the JSON we are given.
-}
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


-- TODO - try to make error messages more precise
{-| Create decoders that depend on previous results. If you are creating
versioned data, you might do something like this:

    info : Decoder Info
    info =
      field "version" int
        |> andThen infoHelp

    infoHelp : Int -> Decoder Info
    infoHelp version =
      case version of
        4 ->
          infoDecoder4

        3 ->
          infoDecoder3

        _ ->
          fail <|
            "Trying to decode info, but version "
            ++ toString version ++ " is not supported."

    -- infoDecoder4 : Decoder Info
    -- infoDecoder3 : Decoder Info
-}
andThen : (a -> Decoder b) -> Decoder a -> Decoder b
andThen =
  Native.Json.andThen


{-| Ignore the JSON and produce a certain Elm value.

    decodeString (succeed 42) "true"    == Ok 42
    decodeString (succeed 42) "[1,2,3]" == Ok 42
    decodeString (succeed 42) "hello"   == Err ... -- this is not a valid JSON string
-}
succeed : a -> Decoder a
succeed =
  Native.Json.succeed


{-| Ignore the JSON and make the decoder fail. This can be useful if you want
to give a custom error message in some case, maybe at the end of a `oneOf`.

    nonEmptyList : Decoder a -> Decoder (List a)
    nonEmptyList decoder =
      let
        checkLength length =
          if length == 0 then
            fail "Cannot accept empty arrays."

          else
            list decoder
      in
        field "length" int
          |> andThen checkLength

    decodeString (nonEmptyList int) "[1,2,3]" == Ok [1,2,3]
    decodeString (nonEmptyList int) "[]"      == Err "... Cannot accept empty arrays."
-}
fail : String -> Decoder a
fail =
  Native.Json.fail

