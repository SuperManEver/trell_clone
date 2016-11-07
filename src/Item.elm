module Item exposing (..)

import Html as Html exposing (..)
import Html.Attributes exposing (class, value)
import Html.Events exposing (onClick, onInput)

-- MODEL 

type alias Model = 
  { id : Int 
  , text : String
  , editing : Bool
  }

newItem : Int -> String -> Model
newItem id str = 
  { id = id
  , text = str
  , editing = False
  }


-- UPDATE
type Msg 
  = NoOp
  | DeleteItem Int
  | EditItem Int
  | UpdateText Int String

update : Msg -> List Model -> (List Model, Cmd Msg)
update msg model =
  case msg of 
    NoOp -> 
      (model, Cmd.none)

    DeleteItem id -> 
      let 
        newModel = List.filter (\ item -> not (item.id == id)) model
      in
        newModel ! []

    EditItem id -> 
      let 
        newModel = List.map (\ item -> if item.id == id then {item | editing = True} else item) model
      in
        newModel ! []

    UpdateText id str -> 
      let 
        newModel = List.map (\ item -> if item.id == id then {item | text = str} else item) model
      in 
        newModel ! []

-- VIEW 

itemView : Model -> Html Msg 
itemView {id, text, editing} = 
  let 
    updateText str = UpdateText id str

    itemDisplay = 
      if editing
      then input [ value text, onInput updateText ] []
      else p [ class "inline-block", onClick (EditItem id) ] [ Html.text text ]
  in 
    div [ class "inline-block" ] 
      [ itemDisplay 
      ]

view : Model -> Html Msg 
view model = 
  div [ class "item" ] 
    [ itemView model
    , span [ class "glyphicon glyphicon-trash", onClick (DeleteItem model.id) ] []
    ]