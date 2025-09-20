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


sampleCode = "https://sky.thatg.co/o=8RZ7ImJvZHkiOnsiaWQiOjE5OTYzODIwMjYsInRleCI6MCwicGF0CABBbWFzawkAwGR5ZSI6Iihub25lLAUAlCkifSwid2luZ0YAnzI2Njg5ODM0MUUAHUVoYWlyRQCfOTIyODcwODA0RgAdAh4AA0YAjzU5MjUwMTQz0QAeNW5lY0YAnzM4MDA4ODQ2OdIAHkVmZWV0XQGfNTMyMDkzOTkwGAEeNW9ybowAnzY4MDQ5OTIyOYwAHjVhY2VGAJ81OTk5ODY4ODVGAB1FcHJvcEYAjzQxOTI3NDI40gAfFWEXAZ80MTc3OTQzNTajAR7wG2hlaWdodCI6MS40ODgyNTM4LCJzY2FsZSI6LTAuMDM1ODE0NTUsInZvafkAsTEsImF0dGl0dWRlUwAxc2VlIQLwCDIwMiwicmVmcmVzaHZlcnNpb24iOjB9"

init : () -> (Model, Cmd Msg)
init _ =
  ( { codeEntry = "" --sampleCode
    , urlText = Nothing
    , output = Nothing
    , outfitHeight = Nothing
    , outputView = View.NoOutput
    }
  , Cmd.none
  --, Lz4.decompressBlock (sampleCode |> removeUrl)
  )

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
      , Lz4.decompressBlock (model.codeEntry |> removeUrl)
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
  String.replace "https://sky.thatg.co/o=" "" urlText

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


