module Deck exposing (..)

import Dom
import Html exposing (..)
import Html.Attributes as Attr exposing (class, value, id)
import Html.Events exposing (onClick, onInput, on, keyCode)
import Json.Decode as Json
import Task
import Random
import Item 

-- MODEL

type alias Model = 
  { id : Int
  , name : String
  , items : List Item.Model
  , showAddItem : Bool
  , field : String
  , editDeckName : Bool
  }

newDeck : Int -> String -> Bool -> List Item.Model -> Model 
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
  | AddItem (Int, Int)
  | ItemMsg Int Item.Msg


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

      AddItem (deck_id, item_id) -> 
        let 
          newModel = 
            updateModel 
              (\ deck -> 
                { deck 
                | items = deck.items ++ [Item.newItem item_id deck.field]
                , showAddItem = False
                , field = ""
                }) 
              deck_id
        in
          (newModel, Cmd.none)

      CreateItem id -> 
        let addItem item_id = AddItem (id, item_id)
        in
          (model, Random.generate addItem (Random.int 1 100000))

      UpdateDeckName id str -> 
        let 
          newModel = updateModel (\ deck -> {deck | name = str}) id
        in
          (newModel, Cmd.none)

      EditDeckName id -> 
        let 
          newModel = toggleEditDeckName id
          focus = Dom.focus ("edit-deck-name-" ++ toString id)
        in 
          (newModel, Task.perform (\_ -> NoOp) (\_ -> NoOp) focus)

      SaveNewDeckName id -> 
        let 
          newModel = toggleEditDeckName id
        in
          (newModel, Cmd.none)

      ItemMsg id itemMsg -> 
        let 
          -- (updatedItems, itemCmd) = Item.update itemMsg model.items
          -- newModel = updateModel (\ deck -> {deck | items = updatedItems}) id 
          -- doesn't work
          -- deck = List.head << List.filter (\ deck -> deck.id == id) model 
          deck model = 
            model 
              |> List.filter (\ deck -> deck.id == id)
              |> List.head
        in
          -- ({ model | items = updatedItems }, Cmd.map ItemMsg itemCmd)
          -- (newModel, Cmd.map (ItemMsg id) itemCmd)
          case deck model of 
            Nothing -> 
              (model, Cmd.none)

            Just deck -> 
              let
                (updatedItems, itemCmd) = Item.update itemMsg deck.items
              in
                (updateModel (\ deck -> if deck.id == id then {deck | items = updatedItems} else deck), Cmd.map ItemMsg itemCmd)


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
    updateField str = UpdateField id str
  in
    if showAddItem
    then 
      div [ class "add-deck" ] 
        [ input [ value field, onInput updateField, onEnter (CreateItem id), Attr.id ("addItemId-" ++ toString id) ] [] 
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


editDeckNameView : Model -> Html Msg
editDeckNameView {id, editDeckName, name} = 
  let 
    updateDeckName str = UpdateDeckName id str
  in 
    if editDeckName
    then 
      input 
        [ class "deck-name-input"
        , value name
        , onInput updateDeckName
        , onEnter (SaveNewDeckName id)
        , Attr.id ("edit-deck-name-" ++ toString id) ] []
    else p [ onClick (EditDeckName id) ] [ text name ]


view : Model -> Html Msg  
view model = 
  div [ class "deck" ] 
    [ editDeckNameView model
    , itemsListView model
    , addItemView model
    ]
