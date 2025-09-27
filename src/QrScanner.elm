port module QrScanner exposing (QrCode(..), scanFile, scanCamera, scanned)

import Json.Decode as Decode

type QrCode
  = Scanned String
  | Error String
  | CommunicationError Decode.Error

scanFile : Decode.Value -> Cmd msg
scanFile = qrScanFile

scanCamera : Cmd msg
scanCamera = qrScanCamera ()

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

port qrScanFile : Decode.Value -> Cmd msg
port qrScanCamera : () -> Cmd msg
port qrFileScanned : (String -> msg) -> Sub msg
port qrError : (Decode.Value -> msg) -> Sub msg
