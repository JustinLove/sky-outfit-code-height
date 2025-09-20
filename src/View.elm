module View exposing (Msg(..), OutfitHeight, OutputView(..), view, document)

import PortData exposing (PortData(..))

import Element exposing (..)
import Element.Background as Background
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import Html.Attributes

type Msg
  = None
  | CodeText String
  | Decode
  | SelectOutputView OutputView

type alias OutfitHeight =
  { height : Float
  , scale : Float
  }

type OutputView
 = NoOutput
 | RawOutput
 | PrettyOutput
 | DecodedValues

--document : (Msg -> msg) -> model -> Browser.Document msg
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
      column [ height fill, width fill ]
        [ inputArea model.codeEntry
        , intermediateArea model
        , outputArea model
        ]

inputArea codeEntry =
  column [ padding 2, spacing 10, width fill ]
    [ Input.multiline
      [ padding 2
      , Background.color input
      , width (fill |> minimum 400)
      , height (px 200)
      , alignLeft
      , htmlAttribute (Html.Attributes.class "line-break-anywhere")
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

intermediateArea model =
  column [ width fill ]
    [ case model.urlText of
        Just url ->
          paragraph
            [ htmlAttribute (Html.Attributes.class "line-break-anywhere")
            ]
            [ text url ]
        Nothing ->
          none
    ]

outputArea model =
  column [ width fill, padding 10 ]
    [ case model.outputView of
      NoOutput ->
        none
      _ ->
        outputAreaButtons model
    , case model.outputView of
      NoOutput ->
        none
      RawOutput ->
        displayPortData rawOutputArea model.output
      PrettyOutput ->
        displayPortData prettyOutputArea model.prettyOutput
      DecodedValues ->
        displayPortData heightArea model.outfitHeight
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


showError : String -> Element msg
showError body =
  paragraph
    [ htmlAttribute (Html.Attributes.class "line-break-anywhere")
    ]
    [ text body ]

displayPortAccessory : PortData a -> Element msg -> Element msg
displayPortAccessory portData accessory =
  case portData of
    NotRequested ->
      none
    NotAvailable ->
      none
    Loading ->
      none
    Data data ->
      accessory
    Failed error ->
      accessory

outputAreaButtons model =
  el
    [ width fill
    ]
    <| row
      [ padding 10
      , spacing 20
      , centerX
      ]
      [ tabHeader "ico" "Height" SelectOutputView DecodedValues model.outputView
        |> displayPortAccessory model.outfitHeight
      , tabHeader "ico" "JSON" SelectOutputView PrettyOutput model.outputView
        |> displayPortAccessory model.prettyOutput
      , tabHeader "ico" "Raw" SelectOutputView RawOutput model.outputView
        |> displayPortAccessory model.output
      ]

rawOutputArea : String -> Element msg
rawOutputArea output =
  paragraph
    [ htmlAttribute (Html.Attributes.class "line-break-anywhere")
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

tabHeader : String -> String -> (mode -> Msg) -> mode -> mode -> Element Msg
tabHeader ico name tagger mode current =
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

foreground = rgb 0.9 0.9 0.9
background = rgb 0.1 0.1 0.1
highlight = rgb 0.4 0.4 0.4
input = rgb 0 0 0

