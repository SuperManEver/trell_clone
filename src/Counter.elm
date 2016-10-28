module Counter exposing (..)

import Html exposing (Html, div, p, text, button)
import Html.Events exposing (onClick)
import Html.Attributes exposing (class)

type alias Model = 
  { id : Int
  , value : Int
  }

type Msg = Increment Int | Decrement Int

update : Msg -> Maybe Model -> (Model, Cmd Msg)
update msg model =
  case msg of 
    Increment id -> 
      case model of 
        Just value -> 
          ({ model | value = model.value + 1}, Cmd.none)
        Nothing ->
          ({ id = 0, value = 0}, Cmd.none)

    Decrement id -> 
      case model of 
        Just value -> 
          ({ model | value = model.value - 1}, Cmd.none)
        Nothing ->
          ({ id = 0, value = 0}, Cmd.none)

view : Model -> Html Msg
view {id, value} = 
  let
    strValue = toString value
  in
    div [ class "counter" ] 
      [ p [ class "counter-value" ] [ text strValue ]
      , button [ onClick (Decrement id) ] [ text "-" ]
      , button [ onClick (Increment id) ] [ text "+" ] 
      ]