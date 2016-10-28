module Counter exposing (..)

import Html exposing (Html, div, p, text, button)
import Html.Events exposing (onClick)
import Html.Attributes exposing (class)

type alias Model = { value : Int }

type Msg = Increment | Decrement

update : Msg -> Model -> (Model, Cmd Msg)
update msg {value} =
  case msg of 
    Increment -> 
      ({ model | value = value + 1}, Cmd.none)

    Decrement -> 
      ({ model | value = value - 1}, Cmd.none)


model : Model
model = { value = 0 }


view : Model -> Html Msg
view {value} = 
  let
    strValue = toString value
  in
    div [ class "counter" ] 
      [ p [ class "counter-value" ] [ text strValue ]
      , button [ onClick Decrement ] [ text "-" ]
      , button [ onClick Increment ] [ text "+" ] 
      ]