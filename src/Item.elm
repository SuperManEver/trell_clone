module Item exposing (..)

import Html exposing (..)
import Html.Attributes exposing (class)

-- MODEL 

type alias Model = 
  { id : Int 
  , text : String
  }

newItem : Int -> String -> Model
newItem id str = 
  { id = id
  , text = str
  }


-- UPDATE
type Msg 
  = NoOp


update : Msg -> List Model -> (List Model, Cmd Msg)
update msg model =
  case msg of 
    NoOp -> 
      (model, Cmd.none)


-- VIEW 
view : Model -> Html Msg 
view model = 
  div [ class "item" ] [ text model.text ]