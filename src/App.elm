import Html exposing (..)
import Html.Attributes exposing (class)
import Html.App exposing (program)
import Html.Events exposing (onClick)
import Random 

main = 
  program
    { init = init
    , view = view 
    , update = update
    , subscriptions = subscriptions
    }

init : (Model, Cmd Msg)
init = 
  let 
    counters = [ { id = 1, value = 0 }, { id = 2, value = 0 } ]
  in 
    (counters, Cmd.none)

-- Type Declarations
type alias CounterModel = 
  { id : Int 
  , value : Int 
  }

type alias Model = List CounterModel
type Msg = Increment Int | Decrement Int | AddCounter Int | RemoveCounter | CreateCounter
-- Type Declarations

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = 
  let 
    updateItem operand id model = 
      List.map 
        (\item -> if item.id == id then { item | value = operand item.value 1 } else item ) 
        model

    increaseItem = updateItem (+)
    decreaseItem = updateItem (-)
    tl = List.drop 1
  in 
    case msg of 
      Increment id -> 
        (increaseItem id model, Cmd.none) 

      Decrement id -> 
        (decreaseItem id model, Cmd.none)

      AddCounter id -> 
        ((CounterModel id 0)::model, Cmd.none) 

      RemoveCounter -> 
        (tl model, Cmd.none)

      CreateCounter -> 
        (model, Random.generate AddCounter (Random.int 1 100000))


subscriptions : Model -> Sub Msg 
subscriptions model =
  Sub.none

counterView : CounterModel -> Html Msg
counterView {id, value} = 
  div [ class "counter" ] 
    [ p [ class "counter-value" ] [ text (toString value) ]  
    , button [ onClick <| Increment id ] [ text "+" ]
    , button [ onClick <| Decrement id ] [ text "-" ]
    ]

countersDisplay : Model -> Html Msg
countersDisplay model = 
  let 
    counters = List.map counterView model
  in
    case counters of 
      [] -> 
        div [] [ text "No counters" ]

      _ -> 
        div [ class "counters-holder" ] counters

view : Model -> Html Msg
view model = 
  div [] 
    [ button [ onClick CreateCounter ] [ text "Add" ]
    , button [ onClick RemoveCounter] [ text "Remove" ]
    , countersDisplay model
    ]

    