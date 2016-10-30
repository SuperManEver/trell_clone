import Html exposing (..)
import Html.App exposing (program)
import Html.Attributes exposing (value, class, placeholder, type')
import Html.Events exposing (onInput, onClick)

main = program
  { init = init 
  , view = view 
  , update = update 
  , subscriptions = subscriptions 
  }

init : (Model, Cmd Msg) 
init = 
  (Model [] "", Cmd.none)

type alias Task = 
  { text : String 
  , completed : Bool 
  }

type alias Model = 
  { tasks : List Task
  , inputValue : String 
  }

type Msg = UpdateInput String | AddTask

subscriptions : Model -> Sub Msg
subscriptions model = 
  Sub.none


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =  
  case msg of 
    UpdateInput val -> 
      ({model | inputValue = val}, Cmd.none)

    AddTask -> 
      let 
        newTask = Task model.inputValue False
      in
        ({ model | tasks = newTask::model.tasks }, Cmd.none)

view : Model -> Html Msg 
view {inputValue, tasks} = 
  let 
    taskItem task = div [] [ p [] [ text task.text ] ]
    tasksList = List.map (\ task -> taskItem task ) tasks
  in 
    div [ class "tasks-container" ]  
      [ div [ class "input-group" ] 
        [ input [ value inputValue, onInput UpdateInput, class "form-control", type' "text", placeholder "Enter task ..." ] []
        , span [ class "input-group-btn" ] 
          [ button [ onClick AddTask, class "btn btn-primary", type' "button" ] [ text "Add" ] ] 
        ]
      , div [ class "tasks-list" ] tasksList
      ]



