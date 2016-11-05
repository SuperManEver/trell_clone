module Deck exposing (..)

import Html exposing (..)
import Html.Attributes exposing (class, value, id)
import Html.Events exposing (onClick, onInput, on, keyCode)


-- MODEL
type alias Item = 
  { id : Int 
  , text : String
  }


type alias Model = 
  { id : Int
  , name : String
  , items : List Item
  , showAddCard : Bool
  }


newDeck : Int -> String -> Model 
newDeck id name = 
  { id = id
  , name = name
  , items = []
  , showAddCard = False
  }  


-- UPDATE 
type Msg 
  = NoOp
  | ShowAddCard Int 
  | HideAddCard Int


update : Msg -> List Model -> (List Model, Cmd Msg)
update msg model = 
  case msg of 
    NoOp -> 
      (model, Cmd.none)

    ShowAddCard id -> 
      (model, Cmd.none)

    HideAddCard id -> 
      (model, Cmd.none)


view : Model -> Html Msg  
view model = 
  div [ class "deck" ] 
    [ p [] [ text model.name ]
    , button [ class "new-card btn btn-link", onClick (ShowAddCard model.id) ] [ text "Add a card ..."]  
    ]
