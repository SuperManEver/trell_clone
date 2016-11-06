module Deck exposing (..)

import Dom
import Html exposing (..)
import Html.Attributes exposing (class, value, id)
import Html.Events exposing (onClick, onInput, on, keyCode)
import Json.Decode as Json
import Task

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
  , editDeckName : Bool
  }


newDeck : Int -> String -> Bool -> List Item -> Model 
newDeck id name show items = 
  { id = id
  , name = name
  , items = items
  , showAddItem = show
  , field = ""
  , editDeckName = False
  }  


-- UPDATE 
type Msg 
  = NoOp
  | ShowAddItem Int 
  | HideAddItem Int
  | UpdateField Int String
  | CreateItem Int
  | EditDeckName Int
  | UpdateDeckName Int String
  | SaveNewDeckName Int


update : Msg -> List Model -> (List Model, Cmd Msg)
update msg model = 
  let 
    updateModel diff id = 
      List.map (\ deck -> if deck.id == id then diff deck else deck ) model

    toggleEditDeckName = 
      updateModel (\ deck -> {deck | editDeckName = not deck.editDeckName }) 
  in 
    case msg of 
      NoOp -> 
        (model, Cmd.none)

      ShowAddItem id -> 
        let 
          newModel = updateModel (\ deck -> { deck | showAddItem = True }) id
          focus = Dom.focus ("addItemId-" ++ toString id)
        in
          (newModel, Task.perform (\_ -> NoOp) (\_ -> NoOp) focus)

      UpdateField id str -> 
        let 
          newModel = updateModel (\ deck -> { deck | field = str }) id
        in
          (newModel, Cmd.none)

      HideAddItem id -> 
        let 
          newModel = updateModel (\ deck -> { deck | showAddItem = False }) id
        in 
          (newModel, Cmd.none)

      CreateItem id -> 
        let 
          newModel = 
            updateModel 
              (\ deck -> {deck | items = deck.items ++ [newItem deck.field], showAddItem = False, field = ""}) 
              id
        in
          (newModel, Cmd.none)

      UpdateDeckName id str -> 
        let 
          newModel = updateModel (\ deck -> {deck | name = str}) id
        in
          (newModel, Cmd.none)

      EditDeckName id -> 
        let 
          newModel = toggleEditDeckName id
          -- focus = Dom.focus ("addItemId-" ++ toString id)
        in 
          (newModel, Cmd.none)

      SaveNewDeckName id -> 
        let 
          newModel = toggleEditDeckName id
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
addItemView model = 
  let 
    updateField str = UpdateField model.id str
  in
    if model.showAddItem
    then 
      div [ class "add-deck" ] 
        [ input [ value model.field, onInput updateField, onEnter (CreateItem model.id), id ("addItemId-" ++ toString model.id) ] [] 
        , div [ class "controls" ] 
          [ button [ class "btn btn-success btn-sm", onClick (CreateItem model.id) ] [ text "Add" ]
          , span [ class "glyphicon glyphicon-remove", onClick (HideAddItem model.id) ] []
          ]
        ]
    else 
      button [ class "new-card btn btn-link", onClick (ShowAddItem model.id) ] [ text "Add a card ..."]  
 

itemsListView : Model -> Html Msg 
itemsListView {items} = 
  let 
    xs = List.map (\ item -> div [ class "item" ] [ text item.text ]) items
  in
    div [] xs

editDeckNameView : Model -> Html Msg
editDeckNameView {id, editDeckName, name} = 
  let 
    updateDeckName str = UpdateDeckName id str
  in 
    if editDeckName
    then input [ class "deck-name-input", value name, onInput updateDeckName, onEnter (SaveNewDeckName id) ] []
    else p [ onClick (EditDeckName id) ] [ text name ]

view : Model -> Html Msg  
view model = 
  div [ class "deck" ] 
    [ editDeckNameView model
    , itemsListView model
    , addItemView model
    ]
