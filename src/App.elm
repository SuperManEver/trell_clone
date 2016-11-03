import Dom
import Html exposing (..)
import Html.App exposing (program)
import Html.Attributes exposing (class, value, id)
import Html.Events exposing (onClick, onInput, on, keyCode)
import Json.Decode as Json
import Task

main = program 
  { init = init 
  , view = view
  , update = update
  , subscriptions = \_ -> Sub.none
  }


init : (Model, Cmd Msg)
init = 
  defaultModel ! []


-- MODEL 

type alias Deck = 
  { id : Int
  , name : String
  }

type alias Model = 
  { decks : List Deck
  , showAddDeck : Bool
  , deckNameField : String
  }

defaultModel : Model 
defaultModel =
  { decks = [{id = 2, name = "Work"}, { id = 3, name="Hobby"}]
  , showAddDeck = True
  , deckNameField = ""
  }

newDeck : String -> Deck 
newDeck name = 
  { id = 1 
  , name = name
  }


-- UPDATE

type Msg 
  = NoOp
  | ShowAddDeck
  | HideAddDeck
  | UpdateDeckNameField String
  | CreateDeck


update : Msg -> Model -> (Model, Cmd Msg)
update msg model = 
  case msg of 
    NoOp -> 
      model ! []

    ShowAddDeck -> 
      let 
        focus = Dom.focus "newDeckInput"
      in 
        {model | showAddDeck = True } ! [Task.perform (\_ -> NoOp) (\_ -> NoOp) focus]

    HideAddDeck -> 
      {model | showAddDeck = False, deckNameField = "" } ! []

    UpdateDeckNameField val -> 
      {model | deckNameField = val } ! []

    CreateDeck -> 
      {model 
        | decks = model.decks ++ [newDeck model.deckNameField]
        , deckNameField = ""
        , showAddDeck = False} ! []

onEnter : Msg -> Attribute Msg
onEnter msg =
  let
    tagger code =
      if code == 13 then
        msg
      else
        NoOp
  in
    on "keydown" (Json.map tagger keyCode)

-- VIEW
addDeckView : Model -> Html Msg 
addDeckView {showAddDeck, deckNameField} = 
  if showAddDeck 
  then 
    div [ class "deck add-deck" ] 
      [ input [ value deckNameField, onInput UpdateDeckNameField, onEnter CreateDeck, id "newDeckInput" ] [] 
      , div [ class "controls" ] 
        [ button [ class "btn btn-success btn-sm", onClick CreateDeck ] [ text "Save" ]
        , span [ class "glyphicon glyphicon-remove", onClick HideAddDeck ] []
        ]
      ]
  else 
    button [ class "new-deck btn btn-link", onClick ShowAddDeck ] [ text "Add a list ..."]  

deckView : Deck -> Html Msg
deckView deck = 
  div [ class "deck" ] 
    [ p [] [ text deck.name ]
    ]

view : Model -> Html Msg 
view model = 
  let 
    decks = List.map (\ deck -> deckView deck ) model.decks
  in
    div [ class "fluid-container" ] 
      [ div [ class "decks-container" ] decks
      , addDeckView model
      ]      
