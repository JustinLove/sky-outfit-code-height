module PortData exposing
  ( PortData(..)
  , withDefault
  , map
  , toMaybe
  , fromResult
  , jsonDecode
  )

import Json.Decode

type PortData a
  = NotRequested
  | NotAvailable
  | Loading
  | Data a
  | Failed String

withDefault : a -> PortData a -> a
withDefault default data =
  case data of
    Data value -> value
    _ -> default

map : (a -> b) -> PortData a -> PortData b
map f data =
  case data of
    NotRequested -> NotRequested
    NotAvailable -> NotAvailable
    Loading -> Loading
    Data value -> Data (f value)
    Failed err -> Failed err

toMaybe : PortData a -> Maybe a
toMaybe data =
  case data of
    NotRequested -> Nothing
    NotAvailable -> Nothing
    Loading -> Nothing
    Data value -> Just value
    Failed err -> Nothing

fromResult : Result String a -> PortData a
fromResult result =
  case result of
    Ok data -> Data data
    Err err -> Failed err

jsonDecode : Json.Decode.Decoder a -> PortData String -> PortData a
jsonDecode decoder portData =
  case portData of 
    NotRequested -> NotRequested
    NotAvailable -> NotAvailable
    Loading -> Loading
    Data value ->
      case Json.Decode.decodeString decoder value of
        Ok data ->
          Data data
        Err error ->
          error
            |> Json.Decode.errorToString
            |> Failed
    Failed err -> Failed err
