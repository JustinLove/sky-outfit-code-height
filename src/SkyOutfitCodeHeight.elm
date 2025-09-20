module SkyOutfitCodeHeight exposing (..)

import Lz4
import View exposing (OutfitHeight)

import Browser
import Json.Decode as Decode

type Msg
  = UI View.Msg
  | BlockDecompressed String
  | DecompressError (Result Decode.Error String)

type alias Model =
  { codeEntry : String
  , urlText : Maybe String
  , output : Maybe String
  , outfitHeight : Maybe OutfitHeight
  , outputView : View.OutputView
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
  , urlText = Nothing
  , output = Nothing
  , outfitHeight = Nothing
  , outputView = View.NoOutput
  }
    |> update (UI View.Decode)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    UI (View.None) -> (model, Cmd.none)
    UI (View.CodeText text) ->
      ( { model | codeEntry = text }, Cmd.none)
    UI (View.Decode) ->
      ( { model
        | urlText = Just model.codeEntry
        }
      , if String.isEmpty model.codeEntry then
          Cmd.none
        else
          Lz4.decompressBlock (model.codeEntry |> removeUrl)
      )
    UI (View.SelectOutputView view) ->
      ( { model | outputView = view }, Cmd.none)
    BlockDecompressed text ->
      let moutfitHeight = decodeHeight text in
      ( { model
        | output = Just text
        , outfitHeight = moutfitHeight
        , outputView = case moutfitHeight of
          Just outfitHeight -> View.DecodedValues
          Nothing -> View.RawOutput
        }
      , Cmd.none
      )
    DecompressError (Ok message)->
      let _ = Debug.log "error" message in
      (model, Cmd.none)
      --({model | layoutStatus = LayoutError "There was an error laying out the graph."}, Cmd.none)
    DecompressError (Err err)->
      let _ = Debug.log "error error" err in
      (model, Cmd.none)
      --({model | layoutStatus = LayoutError "Error decoding error"}, Cmd.none)

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
    ]


