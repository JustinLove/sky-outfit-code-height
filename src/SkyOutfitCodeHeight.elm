module SkyOutfitCodeHeight exposing (..)

import Lz4
import View

import Browser
import Json.Decode

type Msg
  = UI View.Msg
  | BlockDecompressed String
  | DecompressError (Result Json.Decode.Error String)

type alias Model =
  { codeEntry : String
  , urlText : Maybe String
  , output : Maybe String
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
    }
  , Cmd.none
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
    BlockDecompressed text ->
      ( { model
        | output = Just text
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

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    [ Lz4.blockDecompressed BlockDecompressed
    , Lz4.error DecompressError
    ]


