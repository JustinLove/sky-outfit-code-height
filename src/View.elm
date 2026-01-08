module View exposing
  ( Msg(..)
  , OutfitHeight
  , StepId(..)
  , Model
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
import Svg exposing (svg, use)
import Svg.Attributes exposing (xlinkHref)

type Msg
  = None
  | QrCodeFile (List Json.Decode.Value)
  | CodeText String
  | Decode
  | SelectStep StepId
  | BaseHeight String
  | Height String
  | Scale String

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

type alias Model =
  { fileCode : PortData String
  , cameraCode : PortData String
  , hasCamera : Bool
  , codeEntry : String
  , output : PortData String
  , prettyOutput : PortData String
  , outfitHeight : PortData OutfitHeight
  , baseHeightEntry : String
  , heightEntry : String
  , scaleEntry : String
  , currentStep : StepId
  }

document tagger model =
  { title = "Sky Outfit Code Height"
  , body = [Html.map tagger (view model)]
  }

view : Model -> Html Msg
view model =
  layout
    [ width fill
    , Font.color foreground
    , Font.size (scaled 2)
    , Background.color background
    ] <|
      stepsArea model (visibleStepList model.hasCamera)

stepsArea : Model -> List StepId -> Element Msg
stepsArea model id =
  column [ width fill, spacing 10 ]
    (List.map (stepArea model) id)

stepArea : Model -> StepId -> Element Msg
stepArea model id =
  column [ width fill ]
    [ stepHeader (stepIcon id) (stepTitle id) SelectStep id model.currentStep (stepEnabled id model)
    , possiblyHidden (id == model.currentStep)
      <| stepBody id model
    ]

possiblyHidden : Bool -> Element msg -> Element msg
possiblyHidden visible content =
  el
    (if visible then [ width fill ] else [ class "hidden" ])
    content

stepIcon : StepId -> String
stepIcon id =
  case id of
    StepNotice -> "info"
    StepFindingYourCode -> "search"
    StepQrFile -> "file-picture"
    StepQrCamera -> "qrcode"
    StepCodeEntry -> "paste"
    StepRaw -> "embed"
    StepPretty -> "list2"
    StepDecoded -> "text-height"

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

stepEnabled : StepId -> Model -> Bool
stepEnabled id model =
  case id of
    StepNotice -> True
    StepFindingYourCode -> True
    StepQrFile -> dataEnabled model.fileCode
    StepQrCamera -> dataEnabled model.cameraCode
    StepCodeEntry -> True
    StepRaw -> dataEnabled model.output
    StepPretty -> dataEnabled model.prettyOutput
    StepDecoded -> dataEnabled model.outfitHeight

dataEnabled : PortData a -> Bool
dataEnabled portData =
  case portData of
    NotRequested -> True
    NotAvailable -> False
    Loading -> True
    Data _ -> True
    Failed _ -> True

stepBody : StepId -> Model -> Element Msg
stepBody id model =
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
      StepNotice ->
        noticeArea
      StepFindingYourCode ->
        findingYourCodeArea
      StepQrFile ->
        qrFileArea model.fileCode
      StepQrCamera ->
        qrCameraArea model.cameraCode
      StepCodeEntry ->
        inputArea model.codeEntry
      StepRaw ->
        displayPortData rawOutputArea model.output
      StepPretty ->
        displayPortData prettyOutputArea model.prettyOutput
      StepDecoded ->
        displayPortData heightArea model.outfitHeight

noticeArea : Element msg
noticeArea =
  column [ width fill ]
    [ column
      [ width (fill |> maximum 900)
      , centerX
      , spacing 20
      ]
      [ el [ centerX, Font.size (scaled 3) ] <|
        text "For Entertainment Only"
      , paragraph []
        [ text "Outfit codes reveal exact height and scale values of players in "
        , link linkStyles
          { url = "https://www.thatskygame.com/"
          , label = text "Sky: Children of the Light"
          }
        , text ", which I know is a topic of much interest to many skykids."
        ]
      , paragraph [] [ text "However, please remember that while this offers more percision, changing size is no easier than it was before. Please don't go chasing that last fraction." ]
      , el [ centerX, Font.size (scaled 3) ] <|
        text "It is okay be different"
      , el [ centerX, Font.size (scaled 0) ] <|
        paragraph [ ] [ text "This app is not affiliated with, approved, or endorsed by That Game Company." ]
      , displayFooter
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
        , Input.button linkStyles
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
  column [ width fill, spacing 20 ]
  [ el
      [ height (px 300)
      , width fill
      , scrollbarY
      ]
        <| text output
  , column
      [ width (fill |> maximum 900)
      , centerX
      , spacing 20
      ]
      [ el [ centerX, Font.size (scaled 0) ] <|
        paragraph [ ] [ text "Codes contain duplicate 'h' keys. One of the values will not appear in the formatted output." ]
      ]
  ]

heightArea : OutfitHeight -> Element Msg
heightArea outfitHeight =
  column
    [ width fill
    , padding 10
    , spacing 40
    ]
    [ column
      [ width fill
      , spacing 10
      , Font.size (scaled 4)
      ]
      [ valueRow "Height" outfitHeight.height
      , valueRow "Scale" outfitHeight.scale
      ]
    ]

-- this was based on some back calculation of my numbers from https://skykidheight.com/  and it's references
-- but it doesn't match up with npc sizes, and one sample really isn't enough.
calculationArea : String -> String -> String -> Element Msg
calculationArea baseHeightEntry heightEntry scaleEntry =
  column
    [ width fill
    , spacing 20
    ]
    [ el [ centerX, Font.size (scaled 3) ] <|
      text "Speculative Height Calculation"
    , el [ centerX ] <|
      paragraph [] [ text "I'm making this up based on prior work on skyid height. But it seems to come out about right, at least for me." ]
    , el [ centerX ] <| paragraph []
      [ text "("
      , Input.text
        [ width (px (4 * (scaled 1)))
        , Background.color input
        ]
        { onChange = BaseHeight
        , text = baseHeightEntry
        , placeholder = Nothing
        , label = Input.labelBelow [] <| text "Base"
        }
      , text " + "
      , Input.text
        [ width (px (9 * (scaled 1)))
        , Background.color input
        ]
        { onChange = Height
        , text = heightEntry
        , placeholder = Nothing
        , label = Input.labelBelow [] <| text "Height"
        }
      , text ") × (1 + "
      , Input.text
        [ width (px (9 * (scaled 1)))
        , Background.color input
        ]
        { onChange = Scale
        , text = scaleEntry
        , placeholder = Nothing
        , label = Input.labelBelow [] <| text "Scale"
        }
      , text ")"
      ]
    , el [ centerX ] <| paragraph []
      [ Maybe.map2
          (\baseHeight height -> String.fromFloat (baseHeight + height))
          (String.toFloat baseHeightEntry)
          (String.toFloat heightEntry)
        |> Maybe.withDefault "--"
        |> text
      , text " × "
      , Maybe.map
          (\scale -> String.fromFloat (1 + scale))
          (String.toFloat scaleEntry)
        |> Maybe.withDefault "--"
        |> text
      ]
    , Maybe.map3
          (\baseHeight height scale ->
            ((height + baseHeight) * (1 + scale))
              |> truncate
              |> String.fromFloat
          )
          (String.toFloat baseHeightEntry)
          (String.toFloat heightEntry)
          (String.toFloat scaleEntry)
        |> Maybe.withDefault "--"
        |> text
        |> el [ centerX, Font.size (scaled 3) ]
    ]

precision = 10000000

truncate : Float -> Float
truncate x =
  (x * precision)
    |> round
    |> toFloat
    |> (\y -> y / precision)

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
          [ el [ Font.size 16 ] <| icon ico
          , text name
          ]
    }

targetFiles : (List Json.Decode.Value -> msg) -> Json.Decode.Decoder msg
targetFiles tagger =
  (Json.Decode.at ["target", "files"] (Json.Decode.list Json.Decode.value))
    |> Json.Decode.map tagger
    --|> Json.Decode.map (Debug.log "files")

displayFooter : Element msg
displayFooter =
  wrappedRow
    [ Region.footer
    , centerX
    , spacing 20
    , Font.size (scaled 0)
    ]
    [ link []
      { url = "https://github.com/JustinLove/sky-outfit-code-height"
      , label = row [] [ icon "github", text "sky-outfit-code-height" ]
      }
    , link []
      { url = "https://twitter.com/wondible"
      , label = row [] [ icon "twitter", text "@wondible" ]
      }
    , link []
      { url = "https://twitch.tv/wondible"
      , label = row [] [ icon "twitch", text "wondible" ]
      }
    ]

class : String -> Element.Attribute msg
class name =
  htmlAttribute (Html.Attributes.class name)

icon : String -> Element msg
icon name =
  svg [ Svg.Attributes.class ("icon icon-"++name) ]
    [ use [ xlinkHref ("symbol-defs.svg#icon-"++name) ] [] ]
  |> html

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


linkStyles =
  [ Font.underline
  , mouseOver [ Font.color highlight ]
  ]

scaled = modular 16 1.25 >> round
