import Dom
import Html exposing (..)
import Html.App as App exposing (program)
import Html.Attributes exposing (class, value, id)
import Html.Events exposing (onClick, onInput, on, keyCode)
import Json.Decode as Json
import Task
import Deck
import Random
import Item

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

type alias Model = 
  { decks : List Deck.Model
  , showAddDeck : Bool
  , deckNameField : String
  }


defaultModel : Model 
defaultModel =
  { decks = [ Deck.newDeck 1 "Elm" False [], Deck.newDeck 2 "Haskell" True [Item.newItem 1 "Hello", Item.newItem 2 "World"] ]
  , showAddDeck = False
  , deckNameField = ""
  }

-- UPDATE

type Msg 
  = NoOp
  | ShowAddDeck
  | HideAddDeck
  | UpdateDeckNameField String
  | CreateDeck
  | AddDeck Int
  | DeckMsg Deck.Msg


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

    AddDeck id -> 
      {model 
        | decks = model.decks ++ [Deck.newDeck id model.deckNameField False []]
        , deckNameField = ""
        , showAddDeck = False
        } ! []

    CreateDeck -> 
      model ! [ Random.generate AddDeck (Random.int 1 100000) ]

    DeckMsg subMsg -> 
      let 
        (updatedDecks, deckCmd) = Deck.update subMsg model.decks
      in
        ({ model | decks = updatedDecks}, Cmd.map DeckMsg deckCmd)



-- VIEW

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


view : Model -> Html Msg 
view model = 
  let 
    decks = List.map (\ deck -> App.map DeckMsg (Deck.view deck)) model.decks
  in
    div [ class "fluid-container" ] 
      [ div [ class "decks-container" ] decks
      , addDeckView model
      ]      
