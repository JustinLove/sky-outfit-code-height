module SkyOutfitCodeHeight exposing (..)

import Lz4
import PortData exposing (PortData(..))
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
  , output : PortData String
  , outfitHeight : PortData OutfitHeight
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
  , output = NotRequested
  , outfitHeight = NotRequested
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
      let
        output = Data text
        outfitHeight = output |> PortData.jsonDecode heightDecoder
      in
      ( { model
        | output = Data text
        , outfitHeight = outfitHeight
        , outputView = case outfitHeight |> PortData.toMaybe of
          Just _ -> View.DecodedValues
          Nothing -> View.RawOutput
        }
      , Cmd.none
      )
    DecompressError (Ok message)->
      ( { model
        | output = Failed message
        , outfitHeight = NotAvailable
        , outputView = View.RawOutput
        }
      , Cmd.none
      )
    DecompressError (Err err)->
      let _ = Debug.log "error error" err in
      ( { model
        | output = Failed "Error decoding error"
        , outfitHeight = NotAvailable
        , outputView = View.RawOutput
        }
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
    ]


