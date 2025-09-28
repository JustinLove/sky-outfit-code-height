module SkyOutfitCodeHeight exposing (..)

import FormatJson
import Lz4
import PortData exposing (PortData(..))
import QrScanner
import View exposing (OutfitHeight)

import Browser
import Json.Decode as Decode

type Msg
  = UI View.Msg
  | QrScanned QrScanner.QrCode
  | BlockDecompressed Lz4.Block
  | JsonFormatted FormatJson.Formatted

type alias Model =
  { qrCode : PortData String
  , codeEntry : String
  , output : PortData String
  , prettyOutput : PortData String
  , outfitHeight : PortData OutfitHeight
  , currentStep : View.StepId
  }

main = Browser.document
  { init = init
  , update = update
  , subscriptions = subscriptions
  , view = View.document UI
  }

init : String -> (Model, Cmd Msg)
init search =
  { qrCode = (if search == "" then NotRequested else Data search)
  , codeEntry = search
  , output = NotRequested
  , prettyOutput = NotRequested
  , outfitHeight = NotRequested
  , currentStep = View.StepQrFile
  }
    |> update (UI View.None)

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
      }
        |> processSteps
    UI (View.SelectStep step) ->
      changeStep step model
    QrScanned code ->
      { model
      | qrCode = case code of
        QrScanner.Scanned text -> Data text
        QrScanner.Error message -> Failed message
        QrScanner.CommunicationError err -> Failed "Error decoding error"
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
    (newEntry, qrCmd) = processStep model.qrCode Loading processQrCode
    (newRaw, rawCmd) = processStep newEntry model.output processDecode
    (newPretty, prettyCmd) = processStep newRaw model.prettyOutput processFormat
    (newHeight, heightCmd) = processStep newPretty model.outfitHeight processHeight
    dataModel =
      { model
      | codeEntry = newEntry |> PortData.withDefault ""
      , output = newRaw
      , prettyOutput = newPretty
      , outfitHeight = newHeight
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

processQrCode : String -> (PortData String, Cmd msg)
processQrCode codeText =
  ( Data codeText
  , Cmd.none
  )

processDecode : String -> (PortData String, Cmd msg)
processDecode codeEntry =
  ( Loading
  , if String.isEmpty codeEntry then
      Cmd.none
    else
      Lz4.decompressBlock (codeEntry |> removeUrl)
  )

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
  else if isStepComplete model.qrCode then
    View.StepCodeEntry
  else
    View.StepQrFile

isStepComplete : PortData a -> Bool
isStepComplete data =
  case data of
    NotRequested -> False
    NotAvailable -> False
    Loading -> False
    Data _ -> True
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
  Decode.map2 OutfitHeight
    (Decode.field "height" Decode.float)
    (Decode.field "scale" Decode.float)

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    [ QrScanner.scanned QrScanned
    , Lz4.blockDecompressed BlockDecompressed
    , FormatJson.formatted JsonFormatted
    ]
