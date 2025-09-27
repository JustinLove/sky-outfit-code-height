port module FormatJson exposing (Formatted(..), format, formatted)

import Json.Decode as Decode

type Formatted
  = Pretty String
  | Error String
  | CommunicationError Decode.Error

format : String -> Cmd msg
format = formatJson

formatted : (Formatted -> msg) -> Sub msg
formatted tagger =
  Sub.batch
    [ formattedJson (Pretty>>tagger)
    , formatJsonError (portMap>>tagger)
    ]

portMap : Decode.Value -> Formatted
portMap value =
  case Decode.decodeValue errorDecoder value of
    Ok message -> Error message
    Err error -> CommunicationError error

errorDecoder : Decode.Decoder String
errorDecoder =
  Decode.field "message" Decode.string

port formatJson : String -> Cmd msg
port formattedJson : (String -> msg) -> Sub msg
port formatJsonError : (Decode.Value -> msg) -> Sub msg
