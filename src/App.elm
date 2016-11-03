import Html exposing (..)
import Html.App exposing (program)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)

main = program 
  { init = init 
  , view = view
  , update = update
  , subscriptions = \_ -> Sub.none
  }

type Msg 
  = NoOp
  | ShowAddDeck

type alias Deck = 
  { id : Int
  , name : String
  }

type alias Model = 
  { decks : List Deck
  , showAddDeck : Bool
  }

defaultModel : Model 
defaultModel =
  { decks = []
  , showAddDeck = True
  }

init : (Model, Cmd Msg)
init = 
  defaultModel ! []

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = 
  case msg of 
    NoOp -> 
      model ! []

    ShowAddDeck -> 
      {model | showAddDeck = True } ! []

addDeckView : Bool -> Html Msg 
addDeckView show = 
  if show 
  then 
    div [ class "deck add-deck" ] 
      [ input [] [] 
      , div [ class "controls" ] 
        [ button [ class "btn btn-success btn-sm" ] [ text "Save" ]
        , span [ class "glyphicon glyphicon-remove" ] []
        ]
      ]
  else button [ class "new-deck btn btn-link", onClick ShowAddDeck ] [ text "Add a list ..."]  

view : Model -> Html Msg 
view model = 
  div [ class "decks-container" ] 
    [ div [ class "deck" ] [  ]
    , addDeckView model.showAddDeck
    ]      
