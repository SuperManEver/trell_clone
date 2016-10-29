module Counter exposing (..)

import Html exposing (Html, div, p, text, button)
import Html.Events exposing (onClick)
import Html.Attributes exposing (class)
import List

type alias Model = 
  { id : Int
  , value : Int
  }

type Msg = Increment Int | Decrement Int

update : Msg -> List Model -> (List Model, Cmd Msg)
update msg model =
  let 
    updateItem op xs id = 
      List.map (\ c -> if c.id == id then {c | value = op c.value 1} else c ) xs

    increaseItem = updateItem (+) 
    decreaseItem = updateItem (-) 
  in
  case msg of 
    Increment id ->   
      (increaseItem model id, Cmd.none)

    Decrement id -> 
      (decreaseItem model id, Cmd.none)

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