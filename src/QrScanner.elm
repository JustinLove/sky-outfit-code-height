port module QrScanner exposing (Event(..), scanFile, startCamera, stopCamera, event)

import Json.Decode as Decode
import Json.Encode as Encode

type Event
  = FileScanned String
  | FileError String
  | CameraScanned String
  | CameraError String
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

event : (Event -> msg) -> Sub msg
event tagger =
  qrEvent (decodeEvent >> tagger)

decodeEvent : Decode.Value -> Event
decodeEvent value =
  case Decode.decodeValue eventDecoder value of
    Ok code -> code
    Err error -> CommunicationError error

eventDecoder : Decode.Decoder Event
eventDecoder =
  (Decode.field "kind" Decode.string)
    |> Decode.andThen (\kind ->
      case kind of
        "fileScanned" ->
          Decode.map FileScanned resultDecoder
        "fileError" ->
          Decode.map FileError (Decode.field "error" errorDecoder)
        "cameraScanned" ->
          Decode.map CameraScanned resultDecoder
        "cameraError" ->
          Decode.map CameraError (Decode.field "error" errorDecoder)
        "error" ->
          Decode.map Error (Decode.field "error" errorDecoder)
        _ ->
          Decode.fail kind
      )

resultDecoder : Decode.Decoder String
resultDecoder =
  Decode.field "result" (Decode.field "data" Decode.string)

errorFieldDecoder : Decode.Decoder String
errorFieldDecoder =
  Decode.field "error" errorDecoder

errorDecoder : Decode.Decoder String
errorDecoder =
  Decode.oneOf
    [ Decode.field "message" Decode.string
    , Decode.string
    ]

port qrCommand : Decode.Value -> Cmd msg
port qrFileScanned : (String -> msg) -> Sub msg
port qrError : (Decode.Value -> msg) -> Sub msg
port qrEvent : (Decode.Value -> msg) -> Sub msg
