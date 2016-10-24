import Html exposing (..)
import Html.Attributes exposing (class)
import Html.App exposing (beginnerProgram)
import Html.Events exposing (onClick)

main = 
  beginnerProgram
    { model = model
    , view = view 
    , update = update
    }

type alias CounterModel = 
  { id : Int 
  , value : Int 
  }

type alias Model = List CounterModel

type Msg = Increment Int | Decrement Int

model : Model 
model = 
  [ { id = 1, value = 0 }
  , { id = 2, value = 0 }
  ]

update : Msg -> Model -> Model
update msg model = 
  case msg of 
    Increment id -> 
      List.map (\ c -> if c.id == id then { c | value = c.value + 1 } else c ) model 

    Decrement id -> 
      List.map (\ c -> if c.id == id then { c | value = c.value - 1 } else c ) model  

counterView : CounterModel -> Html Msg
counterView {id, value} = 
  div [ class "counter" ] 
    [ p [ class "counter-value" ] [ text (toString value) ]  
    , button [ onClick <| Increment id ] [ text "+" ]
    , button [ onClick <| Decrement id ] [ text "-" ]
    ]

view : Model -> Html Msg
view model = 
  let 
    counters = List.map counterView model
  in
    div [] counters

    