module SkyOutfitCodeHeight exposing (..)

import FormatJson
import Lz4
import PortData exposing (PortData(..))
import View exposing (OutfitHeight)

import Browser
import Json.Decode as Decode

type Msg
  = UI View.Msg
  | BlockDecompressed Lz4.Block
  | JsonFormatted FormatJson.Formatted

type alias Model =
  { codeEntry : String
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
  { codeEntry = search
  , output = NotRequested
  , prettyOutput = NotRequested
  , outfitHeight = NotRequested
  , currentStep = View.StepCodeEntry
  }
    |> update (UI View.Decode)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    UI (View.None) -> (model, Cmd.none)
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
      ( { model | currentStep = step }, Cmd.none)
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
    (newRaw, rawCmd) = processStep (Data model.codeEntry) model.output processDecode
    (newPretty, prettyCmd) = processStep newRaw model.prettyOutput processFormat
    (newHeight, heightCmd) = processStep newPretty model.outfitHeight processHeight
    dataModel =
      { model
      | output = newRaw
      , prettyOutput = newPretty
      , outfitHeight = newHeight
      }
  in
    ( { dataModel
      | currentStep = pickCurrentView dataModel
      }
    , Cmd.batch
      [ rawCmd
      , prettyCmd
      , heightCmd
      ]
    )

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
  else
    View.StepCodeEntry

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
    [ Lz4.blockDecompressed BlockDecompressed
    , FormatJson.formatted JsonFormatted
    ]


