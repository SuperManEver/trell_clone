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

-- Type Declarations
type alias CounterModel = 
  { id : Int 
  , value : Int 
  }

type alias Model = List CounterModel
type Msg = Increment Int | Decrement Int | AddCounter | RemoveCounter
-- Type Declarations

model : Model 
model = 
  [ { id = 1, value = 0 }
  , { id = 2, value = 0 }
  , { id = 3, value = 0 }
  , { id = 4, value = 0 }
  ]

update : Msg -> Model -> Model
update msg model = 
  let 
    updateItem operand id model = 
      List.map 
        (\item -> if item.id == id then { item | value = operand item.value 1 } else item ) 
        model

    increaseItem = updateItem (+)
    decreaseItem = updateItem (-)
  in 
    case msg of 
      Increment id -> 
        increaseItem id model 

      Decrement id -> 
        decreaseItem id model

      AddCounter -> 
        (CounterModel 5 0)::model 

      RemoveCounter -> 
        List.tail model


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
    div [] 
    [ button [ onClick AddCounter ] [ text "Add" ]
    , button [ onClick RemoveCounter ] [ text "Remove" ]
    , div [ class "counters-holder" ] counters
    ]

    