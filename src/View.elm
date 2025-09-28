module View exposing
  ( Msg(..)
  , OutfitHeight
  , StepId(..)
  , stepList
  , view
  , document
  )

import PortData exposing (PortData(..))

import Element exposing (..)
import Element.Background as Background
import Element.Font as Font
import Element.Input as Input
import Json.Decode
import Html exposing (Html)
import Html.Attributes
import Html.Events

type Msg
  = None
  | QrCodeFile (List Json.Decode.Value)
  | CodeText String
  | Decode
  | SelectStep StepId

type alias OutfitHeight =
  { height : Float
  , scale : Float
  }

type StepId
  = StepQrFile
  | StepQrCamera
  | StepCodeEntry
  | StepRaw
  | StepPretty
  | StepDecoded

stepList =
  [ StepQrFile
  , StepQrCamera
  , StepCodeEntry
  , StepRaw
  , StepPretty
  , StepDecoded
  ]

type alias Sidechannel m =
  { m
  | fileCode : PortData String
  , cameraCode : PortData String
  , codeEntry : String
  , output : PortData String
  , prettyOutput : PortData String
  , outfitHeight : PortData OutfitHeight
  }

document tagger model =
  { title = "Sky Outfit Code Height"
  , body = [Html.map tagger (view model)]
  }

-- view : model -> Html Msg
view model =
  layout
    [ width fill
    , Font.color foreground
    , Background.color background
    ] <|
      stepsArea model stepList

--stepsArea : Model -> List StepId -> Element Msg
stepsArea model id =
  column [ width fill ]
    (List.map (stepArea model) id)

--stepArea : Model -> Step Msg -> Element Msg
stepArea model id =
  column [ width fill ]
    [ stepHeader "ico" (stepTitle id) SelectStep id model.currentStep
    , possiblyHidden (id == model.currentStep)
      <| stepBody id model
    ]

possiblyHidden : Bool -> Element msg -> Element msg
possiblyHidden visible content =
  el
    (if visible then [ width fill ] else [ class "hidden" ])
    content

stepTitle : StepId -> String
stepTitle id =
  case id of
    StepQrFile -> "Upload QR Image"
    StepQrCamera -> "Scan QR Via Camera"
    StepCodeEntry -> "Paste Outfit Code Text"
    StepRaw -> "Raw Decoded Value"
    StepPretty -> "Formatted JSON"
    StepDecoded -> "Height"

stepBody : StepId -> Sidechannel m -> Element Msg
stepBody id sidechannel =
  case id of
    StepQrFile -> qrFileBody sidechannel
    StepQrCamera -> qrCameraBody sidechannel
    StepCodeEntry -> inputBody sidechannel
    StepRaw -> rawBody sidechannel
    StepPretty -> prettyBody sidechannel
    StepDecoded -> decodedBody sidechannel

qrFileBody : Sidechannel m -> Element Msg
qrFileBody sidechannel =
  qrFileArea sidechannel.fileCode

qrCameraBody : Sidechannel m -> Element Msg
qrCameraBody sidechannel =
  qrCameraArea sidechannel.cameraCode

inputBody : Sidechannel m -> Element Msg
inputBody sidechannel =
  inputArea sidechannel.codeEntry

rawBody : Sidechannel m -> Element Msg
rawBody sidechannel =
  displayPortData rawOutputArea sidechannel.output

prettyBody : Sidechannel m -> Element Msg
prettyBody sidechannel =
  displayPortData prettyOutputArea sidechannel.prettyOutput

decodedBody : Sidechannel m -> Element Msg
decodedBody sidechannel =
  displayPortData heightArea sidechannel.outfitHeight

qrFileArea : PortData String -> Element Msg
qrFileArea qrCode =
  column [ padding 2, spacing 10, width fill ]
    [ html qrCodeButtonHtml
    , displayPortError qrCode
    ]

qrCodeButtonHtml : Html.Html Msg
qrCodeButtonHtml =
  Html.input
    [ Html.Attributes.type_ "file"
    , Html.Events.on "change" (targetFiles QrCodeFile)
    ]
    []

qrCameraArea : PortData String -> Element Msg
qrCameraArea qrCode =
  column [ padding 2, spacing 10, width fill ]
    [ html <| Html.video
      [ Html.Attributes.id "qrwebcam"
      ] []
    , displayPortError qrCode
    ]

inputArea : String -> Element Msg
inputArea codeEntry =
  column [ padding 2, spacing 10, width fill ]
    [ Input.multiline
      [ padding 2
      , Background.color input
      , width (fill |> minimum 400)
      , height (px 200)
      , alignLeft
      , class "line-break-anywhere"
      , htmlAttribute (Html.Attributes.rows 6)
      ]
      { onChange = CodeText
      , text = codeEntry
      , placeholder = Nothing
      , label = Input.labelAbove [] (text "Outfit code text")
      , spellcheck = False
      }
    , Input.button [ alignRight ]
      { onPress = Just Decode
      , label = text "Decode"
      }
    ]

displayPortData : (a -> Element msg) -> PortData a -> Element msg
displayPortData withData portData =
  case portData of
    NotRequested ->
      text "Not Requested"
    NotAvailable ->
      text "Not Available"
    Loading ->
      text "loading"
    Data data ->
      withData data
    Failed error ->
      showError error

displayPortError : PortData a -> Element msg
displayPortError portData =
  case portData of
    NotRequested ->
      none
    NotAvailable ->
      none
    Loading ->
      text "loading"
    Data data ->
      none
    Failed error ->
      showError error

showError : String -> Element msg
showError body =
  paragraph
    [ class "line-break-anywhere"
    ]
    [ text body ]

rawOutputArea : String -> Element msg
rawOutputArea output =
  paragraph
    [ class "line-break-anywhere"
    ]
    [ text output ]

prettyOutputArea : String -> Element msg
prettyOutputArea output =
  el
    [ height (px 300)
    , width fill
    , scrollbarY
    ]
      <| text output

heightArea : OutfitHeight -> Element msg
heightArea outfitHeight =
  column
    [ width fill
    , padding 10
    , spacing 10
    ]
    [ valueRow "Height" outfitHeight.height
    , valueRow "Scale" outfitHeight.scale
    ]

valueRow : String -> Float -> Element msg
valueRow label value =
  row
    [ width fill
    , spacing 10
    ]
    [ el [ width fill ]
      (el [ alignRight ] (text label))
     
    , el [ width fill ]
      (el [ alignLeft ] (text (value |> String.fromFloat)))
    ]

stepHeader : String -> String -> (mode -> Msg) -> mode -> mode -> Element Msg
stepHeader ico name tagger mode current =
  Input.button [ width fill ]
    { onPress = Just (tagger mode)
    , label = 
      el
        [ width fill
        , Background.color (if mode == current then highlight else background)
        , paddingXY 10 5
        ] <|
        row [ centerX, spacing 6 ]
          --[ el [ Font.size 16 ] <| icon ico
          [ text name
          ]
    }

targetFiles : (List Json.Decode.Value -> msg) -> Json.Decode.Decoder msg
targetFiles tagger =
  (Json.Decode.at ["target", "files"] (Json.Decode.list Json.Decode.value))
    |> Json.Decode.map tagger
    --|> Json.Decode.map (Debug.log "files")

class : String -> Element.Attribute msg
class name =
  htmlAttribute (Html.Attributes.class name)

foreground = rgb 0.9 0.9 0.9
background = rgb 0.1 0.1 0.1
highlight = rgb 0.4 0.4 0.4
input = rgb 0 0 0

