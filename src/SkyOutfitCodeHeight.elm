module SkyOutfitCodeHeight exposing (..)

import FormatJson
import Lz4
import PortData exposing (PortData(..))
import QrScanner
import View exposing (OutfitHeight, Model)

import Browser
import Json.Decode as Decode

type Msg
  = UI View.Msg
  | Qr QrScanner.Event
  | BlockDecompressed Lz4.Block
  | JsonFormatted FormatJson.Formatted

main = Browser.document
  { init = init
  , update = update
  , subscriptions = subscriptions
  , view = View.document UI
  }

init : String -> (Model, Cmd Msg)
init search =
  { fileCode = NotRequested
  , cameraCode = NotRequested
  , hasCamera = False
  , codeEntry = search
  , output = NotRequested
  , prettyOutput = NotAvailable
  , outfitHeight = NotAvailable
  , baseHeightEntry = "10"
  , heightEntry = "0"
  , scaleEntry = "0"
  , currentStep = View.StepNotice
  }
    |> processSteps

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    UI (View.None) -> (model, Cmd.none)
    UI (View.QrCodeFile files) ->
      ( model
      , files
       |> List.map QrScanner.scanFile
       |> Cmd.batch
      )
    UI (View.CodeText text) ->
      ( { model
        | codeEntry = text
        }
      , Cmd.none)
    UI (View.Decode) ->
      { model
      | output = Loading
      , fileCode = NotRequested
      , cameraCode = NotRequested
      }
        |> processSteps
    UI (View.SelectStep step) ->
      changeStep step model
    UI (View.BaseHeight text) ->
      ( { model | baseHeightEntry = text }, Cmd.none )
    UI (View.Height text) ->
      ( { model | heightEntry = text }, Cmd.none )
    UI (View.Scale text) ->
      ( { model | scaleEntry = text }, Cmd.none )
    Qr code ->
      { model
      | fileCode = case code of
        QrScanner.FileScanned text -> Data text
        QrScanner.FileError message -> Failed message
        QrScanner.CameraScanned text -> NotRequested
        QrScanner.CameraError message -> model.fileCode
        QrScanner.HasCamera flag -> model.fileCode
        QrScanner.Error message -> Failed message
        QrScanner.CommunicationError err -> Failed "Error decoding error"
      , cameraCode = case code of
        QrScanner.FileScanned text -> NotRequested
        QrScanner.FileError message -> model.cameraCode
        QrScanner.CameraScanned text -> Data text
        QrScanner.CameraError message -> Failed message
        QrScanner.HasCamera flag ->
          if flag then
            model.cameraCode
          else
            NotAvailable
        QrScanner.Error message -> Failed message
        QrScanner.CommunicationError err -> Failed "Error decoding error"
      , hasCamera = case code of
        QrScanner.HasCamera flag -> flag
        _ -> model.hasCamera
      }
        |> processSteps
    BlockDecompressed block ->
      { model
      | output = case block of
        Lz4.Decompressed text -> Data text
        Lz4.Error message -> Failed message
        Lz4.CommunicationError err -> Failed "Error decoding error"
      }
        |> processSteps
    JsonFormatted formatted ->
      { model
      | prettyOutput = case formatted of
        FormatJson.Pretty text -> Data text
        FormatJson.Error message -> Failed message
        FormatJson.CommunicationError err -> Failed "Error decoding error"
      }
        |> processSteps

processSteps : Model -> (Model, Cmd msg)
processSteps model =
  let
    qrCode = pickQrCode model.fileCode model.cameraCode
    newEntry = if isStepComplete qrCode then qrCode else Data model.codeEntry
    (newRaw, rawCmd) = processStep newEntry model.output processDecode
    (newPretty, prettyCmd) = processStep newRaw model.prettyOutput processFormat
    (newHeight, heightCmd) = processStep newPretty model.outfitHeight processHeight
    dataModel =
      { model
      | codeEntry = newEntry |> PortData.withDefault ""
      , output = newRaw
      , prettyOutput = newPretty
      , outfitHeight = newHeight
      , heightEntry = newHeight
        |> PortData.map (.height>>String.fromFloat)
        |> PortData.withDefault model.heightEntry
      , scaleEntry = newHeight
        |> PortData.map (.scale>>String.fromFloat)
        |> PortData.withDefault model.scaleEntry
      }
    newStep = pickCurrentView dataModel
    (newModel, stepCmd) = changeStep newStep dataModel
  in
    ( newModel
    , Cmd.batch
      [ rawCmd
      , prettyCmd
      , heightCmd
      , stepCmd
      ]
    )

changeStep : View.StepId -> Model -> (Model, Cmd msg)
changeStep step model =
  if model.currentStep == step then
    (model, Cmd.none)
  else if model.currentStep == View.StepQrCamera then
    ({model | currentStep = step}, QrScanner.stopCamera)
  else if step == View.StepQrCamera then
    ({model | currentStep = step}, QrScanner.startCamera)
  else
    ({model | currentStep = step}, Cmd.none)

processStep : PortData a -> PortData b -> (a -> (PortData b, Cmd msg)) -> (PortData b, Cmd msg)
processStep previous data onUpdate =
  case previous of
    NotRequested -> (NotRequested, Cmd.none)
    NotAvailable -> (NotAvailable, Cmd.none)
    Loading -> (NotAvailable, Cmd.none)
    Data value ->
      case data of
        NotRequested -> onUpdate value
        NotAvailable -> onUpdate value
        Loading -> onUpdate value
        Data v -> (data, Cmd.none)
        Failed err -> (data, Cmd.none)
    Failed err -> (NotAvailable, Cmd.none)

pickQrCode : PortData String -> PortData String -> PortData String
pickQrCode a b =
  case (a, b) of
    (Data _, Data "") -> a
    (Data "", Data _) -> b
    (Data _, _) -> a
    (_, Data _) -> b
    (Failed _, _) -> a
    (_, Failed _) -> b
    _ -> a

processQrCode : String -> (PortData String, Cmd msg)
processQrCode codeText =
  ( Data codeText
  , Cmd.none
  )

processDecode : String -> (PortData String, Cmd msg)
processDecode codeEntry =
  if String.isEmpty codeEntry then
    ( NotAvailable, Cmd.none )
  else
    ( Loading, Lz4.decompressBlock (codeEntry |> removeUrl) )

processFormat : String -> (PortData String, Cmd msg)
processFormat text =
  ( Loading
  , FormatJson.format text
  )

processHeight : String -> (PortData OutfitHeight, Cmd msg)
processHeight text =
  ( PortData.jsonDecode heightDecoder (Data text)
  , Cmd.none
  )

pickCurrentView : Model -> View.StepId
pickCurrentView model =
  if isStepComplete model.outfitHeight then
    View.StepDecoded
  else if isStepComplete model.prettyOutput then
    View.StepPretty
  else if isStepComplete model.output then
    View.StepRaw
  else if isStepSuccess model.fileCode then
    View.StepCodeEntry
  else if isStepSuccess model.cameraCode then
    View.StepCodeEntry
  else if model.hasCamera && isStepFailed model.cameraCode then
    View.StepQrCamera
  else if isStepFailed model.fileCode then
    View.StepQrFile
  else
    View.StepNotice

isStepComplete : PortData a -> Bool
isStepComplete data =
  case data of
    NotRequested -> False
    NotAvailable -> False
    Loading -> False
    Data _ -> True
    Failed _ -> True

isStepSuccess : PortData a -> Bool
isStepSuccess data =
  case data of
    NotRequested -> False
    NotAvailable -> False
    Loading -> False
    Data _ -> True
    Failed _ -> False

isStepFailed : PortData a -> Bool
isStepFailed data =
  case data of
    NotRequested -> False
    NotAvailable -> False
    Loading -> False
    Data _ -> False
    Failed _ -> True

removeUrl : String -> String
removeUrl urlText =
  urlText
    |> String.replace "https://sky.thatg.co/o=" ""
    |> String.replace "?o=" ""

decodeHeight : String -> Maybe OutfitHeight
decodeHeight string =
  Decode.decodeString heightDecoder string
    |> Result.toMaybe

heightDecoder : Decode.Decoder OutfitHeight
heightDecoder =
  Decode.oneOf [ heightDecoder2, heightDecoder1 ]

heightDecoder1 : Decode.Decoder OutfitHeight
heightDecoder1 =
  Decode.map2 OutfitHeight
    (Decode.field "height" Decode.float)
    (Decode.field "scale" Decode.float)

-- the format has invalid json - the h key is duplicated for hair? and height. But it seems height is last.
heightDecoder2 : Decode.Decoder OutfitHeight
heightDecoder2 =
  Decode.map2 OutfitHeight
    (Decode.field "h" Decode.float)
    (Decode.field "s" Decode.float)

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    [ QrScanner.event Qr
    , Lz4.blockDecompressed BlockDecompressed
    , FormatJson.formatted JsonFormatted
    ]
