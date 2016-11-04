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
  }

newDeck : String -> Model 
newDeck name = 
  { id = 1
  , name = name
  , items = []
  }  

-- UPDATE 
type Msg = NoOp

update : Msg -> List Model -> (List Model, Cmd Msg)
update msg model = 
  (model, Cmd.none)

view : Model -> Html Msg  
view model = 
  div [ class "deck" ] 
    [ p [] [ text model.name ]
    ]
