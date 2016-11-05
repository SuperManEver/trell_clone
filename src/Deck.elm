module Deck exposing (..)

import Html exposing (..)
import Html.Attributes exposing (class, value, id)
import Html.Events exposing (onClick, onInput, on, keyCode)
import Json.Decode as Json


-- MODEL
type alias Item = 
  { id : Int 
  , text : String
  }

newItem : String -> Item
newItem str = 
  { id = 1
  , text = str
  }


type alias Model = 
  { id : Int
  , name : String
  , items : List Item
  , showAddItem : Bool
  , field : String
  }


newDeck : Int -> String -> Bool -> List Item -> Model 
newDeck id name show items = 
  { id = id
  , name = name
  , items = items
  , showAddItem = show
  , field = ""
  }  


-- UPDATE 
type Msg 
  = NoOp
  | ShowAddItem Int 
  | HideAddItem Int
  | UpdateField Int String
  | CreateItem Int


update : Msg -> List Model -> (List Model, Cmd Msg)
update msg model = 
  case msg of 
    NoOp -> 
      (model, Cmd.none)

    ShowAddItem id -> 
      let 
        newModel = List.map (\ deck -> if deck.id == id then { deck | showAddItem = True } else deck ) model
      in
        (newModel, Cmd.none)

    UpdateField id str -> 
      let 
        newModel = List.map (\ deck -> if deck.id == id then { deck | field = str } else deck) model
      in
        (newModel, Cmd.none)

    HideAddItem id -> 
      let 
        newModel = List.map (\ deck -> if deck.id == id then { deck | showAddItem = False } else deck) model
      in
        (newModel, Cmd.none)

    CreateItem id -> 
      let 
        newModel = 
          List.map 
            (\ deck -> 
              if deck.id == id 
              then 
                { deck | items = deck.items ++ [newItem deck.field]
                , showAddItem = False
                , field = ""
                } 
              else deck) 
            model
      in
        (newModel, Cmd.none)

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

addItemView : Model -> Html Msg 
addItemView {id, showAddItem, field} = 
  let 
    updateField str = 
      UpdateField id str
  in
    if showAddItem
    then 
      div [ class "add-deck" ] 
        [ input [ value field, onInput updateField, onEnter (CreateItem id) ] [] 
        , div [ class "controls" ] 
          [ button [ class "btn btn-success btn-sm", onClick (CreateItem id) ] [ text "Add" ]
          , span [ class "glyphicon glyphicon-remove", onClick (HideAddItem id) ] []
          ]
        ]
    else 
      button [ class "new-card btn btn-link", onClick (ShowAddItem id) ] [ text "Add a card ..."]  
 

itemsListView : Model -> Html Msg 
itemsListView {items} = 
  let 
    xs = List.map (\ item -> div [ class "item" ] [ text item.text ]) items
  in
    div [] xs

view : Model -> Html Msg  
view model = 
  div [ class "deck" ] 
    [ p [] [ text model.name ]
    , itemsListView model
    , addItemView model
    ]
