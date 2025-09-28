port module QrScanner exposing (QrCode(..), scanFile, startCamera, stopCamera, scanned)

import Json.Decode as Decode
import Json.Encode as Encode

type QrCode
  = Scanned String
  | Error String
  | CommunicationError Decode.Error

scanFile : Decode.Value -> Cmd msg
scanFile file =
  Encode.object
    [ ("kind", Encode.string "scanImage")
    , ("file", file)
    ]
    |> qrCommand

startCamera : Cmd msg
startCamera =
  Encode.object
    [ ("kind", Encode.string "startCamera")
    ]
    |> qrCommand

stopCamera : Cmd msg
stopCamera =
  Encode.object
    [ ("kind", Encode.string "stopCamera")
    ]
    |> qrCommand

scanned : (QrCode -> msg) -> Sub msg
scanned tagger =
  Sub.batch
    [ qrFileScanned (Scanned>>tagger)
    , qrError (portMap>>tagger)
    ]

portMap : Decode.Value -> QrCode
portMap value =
  case Decode.decodeValue errorDecoder value of
    Ok code -> code
    Err error -> CommunicationError error

errorDecoder : Decode.Decoder QrCode
errorDecoder =
  Decode.oneOf
    [ Decode.map Error (Decode.field "message" Decode.string)
    , Decode.map Error (Decode.string)
    ]

port qrCommand : Decode.Value -> Cmd msg
port qrFileScanned : (String -> msg) -> Sub msg
port qrError : (Decode.Value -> msg) -> Sub msg
