module View exposing
  ( Msg(..)
  , OutfitHeight
  , StepId(..)
  , view
  , document
  )

import PortData exposing (PortData(..))

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Element.Region as Region
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
  = StepNotice
  | StepFindingYourCode
  | StepQrFile
  | StepQrCamera
  | StepCodeEntry
  | StepRaw
  | StepPretty
  | StepDecoded

stepList =
  [ StepNotice
  , StepFindingYourCode
  , StepQrFile
  , StepQrCamera
  , StepCodeEntry
  , StepRaw
  , StepPretty
  , StepDecoded
  ]

visibleStepList hasCamera =
  List.filter
    (\id -> not (id == StepQrCamera && hasCamera == False))
    stepList

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
    , Font.size (scaled 2)
    , Background.color background
    ] <|
      stepsArea model (visibleStepList model.hasCamera)

--stepsArea : Model -> List StepId -> Element Msg
stepsArea model id =
  column [ width fill, spacing 10 ]
    (List.map (stepArea model) id)

--stepArea : Model -> Step Msg -> Element Msg
stepArea model id =
  column [ width fill ]
    [ stepHeader "ico" (stepTitle id) SelectStep id model.currentStep (stepEnabled id model)
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
    StepNotice -> "Notice"
    StepFindingYourCode -> "Finding Your Code"
    StepQrFile -> "Scan QR Image File"
    StepQrCamera -> "Scan QR Via Camera"
    StepCodeEntry -> "Paste Outfit Code Text"
    StepRaw -> "Raw Decoded Value"
    StepPretty -> "Formatted JSON"
    StepDecoded -> "Height"

stepEnabled : StepId -> Sidechannel m -> Bool
stepEnabled id sidechannel =
  case id of
    StepNotice -> True
    StepFindingYourCode -> True
    StepQrFile -> dataEnabled sidechannel.fileCode
    StepQrCamera -> dataEnabled sidechannel.cameraCode
    StepCodeEntry -> True
    StepRaw -> dataEnabled sidechannel.output
    StepPretty -> dataEnabled sidechannel.prettyOutput
    StepDecoded -> dataEnabled sidechannel.outfitHeight

dataEnabled : PortData a -> Bool
dataEnabled portData =
  case portData of
    NotRequested -> True
    NotAvailable -> False
    Loading -> True
    Data _ -> True
    Failed _ -> True

stepBody : StepId -> Sidechannel m -> Element Msg
stepBody id sidechannel =
  el
    [ width fill
    , paddingEach
      { top = 20
      , right = 10
      , bottom = 10
      , left = 10
      }
    ]
    <| case id of
      StepNotice -> noticeArea
      StepFindingYourCode -> findingYourCodeArea
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

noticeArea : Element Msg
noticeArea =
  column [ width fill ]
    [ column
      [ width (fill |> maximum 900)
      , centerX
      , spacing 20
      ]
      [ el [ centerX, Font.size (scaled 3) ] <|
        text "For Information Only"
      , paragraph [] [ text "Outfit codes reveal exact height and scale values, which I know is a topic of much interest to many skykids." ]
      , paragraph [] [ text "However, please remember that while this offers more percision, changing size is no easier than it was before. If anything, this may reveal how big an effect each skykid's scale has on possible heights. Please don't go chasing that last fraction." ]
      , el [ centerX, Font.size (scaled 3) ] <|
        text "It is okay be different"
      ]
    ]

findingYourCodeArea : Element Msg
findingYourCodeArea =
  column [ width fill, spacing 20 ]
    [ wrappedRow [ padding 2, spacing 10, centerX ]
      [ instructionImage
        { src = "account.png"
        , description = "Go to game settings and press the Account button."
        }
      , instructionImage
        { src = "accountinfo.png"
        , description = "In Account, press the Account Info button."
        }
      , instructionImage
        { src = "outfitqrcode.png"
        , description = "In Account Info, press the Outfit QR Code button."
        }
      ]
    , column
      [ width (fill |> maximum 900)
      , centerX
      , spacing 20
      ]
      [ paragraph [] [ text "Outfit codes contain only a visual description of your character. They do NOT contain your sky-id or other identifable information" ]
      , paragraph [] [ text "All data is processed in your browser. It is not sent to any server." ]
      ]
    ]

instructionImage desc =
  image
    [ Border.width 10
    , Border.rounded 10
    , Border.color control
    ] desc

qrNotice : Element Msg
qrNotice =
  column [ width fill ]
    [ column
      [ width (fill |> maximum 900)
      , centerX
      , spacing 20
      , Font.size (scaled 0)
      ]
      [ paragraph []
        [ text "The outfit QR codes are exteremely dense and may be hard to read. Using a high-resolution display is recommended. If you can't get a code to scan here, you can try using any other QR-scanning program and "
        , Input.button
          [ Font.underline
          , mouseOver [ Font.color highlight ]
          ]
          { onPress = Just (SelectStep StepCodeEntry)
          , label = text "pasting the text below"
          }
        , text "."
        ]
      ]
    ]

qrFileArea : PortData String -> Element Msg
qrFileArea qrCode =
  column [ padding 2, spacing 10, width fill ]
    [ el [ centerX ] <| html qrCodeButtonHtml
    , displayPortError qrCode
    , qrNotice
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
    [ el
      [ width (shrink |> minimum 200)
      , centerX
      ]
        <| html <| Html.video
          [ Html.Attributes.id "qrwebcam"
          ] []
    , displayPortError qrCode
    , qrNotice
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
      , label = Input.labelAbove [] <| text "Outfit code text"
      , spellcheck = False
      }
    , Input.button [ alignRight ]
      { onPress = Just Decode
      , label =
        el
          [ Font.color foreground
          , Font.size (scaled 2)
          , Background.color control
          , mouseOver [ Background.color highlight ]
          , paddingXY 10 5
          ] <| text "Decode"
      }
    ]

displayPortData : (a -> Element msg) -> PortData a -> Element msg
displayPortData withData portData =
  case portData of
    NotRequested ->
      twoPartMessage "Not Requested" "This may be a bug"
    NotAvailable ->
      twoPartMessage "No Data Available" "This proabably means you need to input something in a step above"
    Loading ->
      el [ centerX, centerY ] <| text "Loading"
    Data data ->
      withData data
    Failed error ->
      twoPartMessage "Error" error

displayPortError : PortData a -> Element msg
displayPortError portData =
  case portData of
    NotRequested ->
      none
    NotAvailable ->
      none
    Loading ->
      el [ centerX, centerY ] <| text "Loading"
    Data data ->
      none
    Failed error ->
      twoPartMessage "Error" error

twoPartMessage : String -> String -> Element msg
twoPartMessage header body =
  column [ centerX, centerY ]
    [ el [ centerX, Font.size (scaled 2)] <|
      text header
    , el [ centerX, Font.size (scaled 1)] <|
      paragraph
        [ class "line-break-anywhere"
        ]
        [ text body ]
    ]

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

stepHeader : String -> String -> (mode -> Msg) -> mode -> mode -> Bool -> Element Msg
stepHeader ico name tagger mode current enabled =
  Input.button [ width fill ]
    { onPress = Just (tagger mode)
    , label = 
      el
        [ width fill
        , Font.color (if enabled || mode == current then foreground else deemphasis)
        , Font.size (scaled 3)
        , Region.heading 2
        , Background.color (if mode == current then highlight else control)
        , mouseOver (if enabled then [ Background.color highlight ] else [])
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

{-- https://colorhunt.co/palette/070d591f3c885893d4ceddef
rgb255 7 13 89
rgb255 31 60 136
rgb255 88 147 212
rgb255 206 221 239
--}
--
foreground = rgb255 206 221 239
background = rgb255 7 13 89
highlight = rgb255 88 147 212
control = rgb255 31 60 136
input = rgb255 0 0 0
deemphasis = rgb255 88 147 212
--}

scaled = modular 16 1.25 >> round
