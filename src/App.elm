import Html exposing (..)
import Html.App exposing (program)

main = program 
  { init = init 
  , view = view
  , update = update
  , subscriptions = \_ -> Sub.none
  }

type Msg = NoOp

type alias Deck = 
  { id : Int
  , name : String
  }

type alias Model = 
  { decks : List Deck
  }

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = 
  case msg of 
    NoOp -> 
      model ! []


view : Model -> Html Msg 
view model = 
  div [] []      
