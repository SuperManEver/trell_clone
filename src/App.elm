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
  (Model [ (Task 1 "buy bread" False), (Task 2 "buy milk" False) ] "", Cmd.none)

type alias Task = 
  { id : Int
  , text : String 
  , completed : Bool 
  }

type alias Model = 
  { tasks : List Task
  , inputValue : String 
  }

type Msg = UpdateInput String | AddTask | ToggleTask Int

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
        newTask = Task 3 model.inputValue False
      in
        ({ model | tasks = newTask::model.tasks, inputValue = "" }, Cmd.none)

    ToggleTask id -> 
      let 
        updatedTasks = 
          List.map 
            (\task -> if task.id == id then {task | completed = not task.completed} else task) 
            model.tasks
      in
        ({ model | tasks = updatedTasks }, Cmd.none)


taskInput inputValue = 
  div [ class "input-group" ] 
    [ input [ value inputValue, onInput UpdateInput, class "form-control", type' "text", placeholder "Enter task ..." ] []
    , span [ class "input-group-btn" ] 
      [ button [ onClick AddTask, class "btn btn-primary", type' "button" ] [ text "Add" ] ] 
    ]


taskItem task = 
  let 
    textClasses = if task.completed then "completed" else ""
  in
  div [ class "task-item" ] 
    [ input [ type' "checkbox", onClick (ToggleTask task.id) ] []
    , p [ class textClasses ] [ text task.text ] 
    ]


view : Model -> Html Msg 
view {inputValue, tasks} = 
  let 
    tasksList = List.map (\ task -> taskItem task ) tasks
  in 
    div [ class "tasks-container" ]  
      [ taskInput inputValue
      , div [ class "tasks-list" ] tasksList
      ]



