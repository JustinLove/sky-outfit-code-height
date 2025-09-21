module SkyOutfitCodeHeight exposing (..)

import FormatJson
import Lz4
import PortData exposing (PortData(..))
import View exposing (OutfitHeight)

import Browser
import Json.Decode as Decode

type Msg
  = UI View.Msg
  | BlockDecompressed String
  | DecompressError (Result Decode.Error String)
  | JsonFormatted String
  | JsonError (Result Decode.Error String)

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
    BlockDecompressed text ->
      { model
      | output = Data text
      }
        |> processSteps
    DecompressError (Ok message)->
      { model
      | output = Failed message
      }
        |> processSteps
    DecompressError (Err err)->
      let _ = Debug.log "error error" err in
      { model
      | output = Failed "Error decoding error"
      }
        |> processSteps
    JsonFormatted pretty ->
      { model
      | prettyOutput = Data pretty
      }
        |> processSteps
    JsonError (Ok message)->
      { model
      | prettyOutput = Failed message
      }
        |> processSteps
    JsonError (Err err)->
      let _ = Debug.log "error error" err in
      { model
      | prettyOutput = Failed "Error decoding error"
      }
        |> processSteps

processSteps : Model -> (Model, Cmd msg)
processSteps model =
  let
    (newRaw, rawCmd) = processStep (Data model.codeEntry) model.output processDecode
    (newPretty, prettyCmd) = processStep newRaw model.prettyOutput processFormat
    (newHeight, heightCmd) = processStep newPretty model.outfitHeight processHeight
  in
    ( { model
      | output = newRaw
      , prettyOutput = newPretty
      , outfitHeight = newHeight
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
        Data v -> (Data v, Cmd.none)
        Failed err -> (Failed err, Cmd.none)
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
    , Lz4.error DecompressError
    , FormatJson.formatted JsonFormatted
    , FormatJson.error JsonError
    ]


