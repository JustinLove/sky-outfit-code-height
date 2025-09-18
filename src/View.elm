module View exposing (Msg(..), OutfitHeight, view, document)

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

type alias OutfitHeight =
  { height : Float
  , scale : Float
  }

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

outputArea model =
  column [ width fill ]
    [ case model.urlText of
        Just url ->
          paragraph
            [ htmlAttribute (Html.Attributes.class "line-break-anywhere")
            ]
            [ text url ]
        Nothing ->
          none
    , case model.output of
        Just output ->
          paragraph
            [ htmlAttribute (Html.Attributes.class "line-break-anywhere")
            ]
            [ text output ]
        Nothing ->
          none
    , case model.outfitHeight of
        Just outfitHeight ->
          heightArea outfitHeight
        Nothing ->
          none
    ]

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

foreground = rgb 0.9 0.9 0.9
background = rgb 0.1 0.1 0.1
input = rgb 0 0 0
