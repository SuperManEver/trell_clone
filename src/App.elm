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

type alias Model = 
  { value : Int }

type Msg = Increment | Decrement

model : Model 
model = 
  { value = 0 }

update : Msg -> Model -> Model
update msg model = 
  case msg of 
    Increment -> 
      { model | value = model.value + 1 }   

    Decrement -> 
      { model | value = max (model.value - 1) 0 }

counterView value = 
  div [ class "counter" ] 
    [ p [ class "counter-value" ] [ text value ]  
    , button [ onClick Increment ] [ text "+" ]
    , button [ onClick Decrement ] [ text "-" ]
    ]

view model = 
  let 
    counterValue = toString model.value
  in 
    counterView counterValue
    