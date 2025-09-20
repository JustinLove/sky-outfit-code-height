port module FormatJson exposing (format, formatted, error)

import Json.Decode as Decode

format : String -> Cmd msg
format = formatJson

formatted : (String -> msg) -> Sub msg
formatted = formattedJson

error : (Result Decode.Error String -> msg) -> Sub msg
error msg =
  formatJsonError (portMap msg)

portMap : (Result Decode.Error String -> msg) -> Decode.Value -> msg
portMap msg =
  (Decode.decodeValue errorDecoder) >> msg

errorDecoder : Decode.Decoder String
errorDecoder =
  Decode.field "message" Decode.string

port formatJson : String -> Cmd msg
port formattedJson : (String -> msg) -> Sub msg
port formatJsonError : (Decode.Value -> msg) -> Sub msg
