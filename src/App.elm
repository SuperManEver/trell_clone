import Html exposing (..)
import Html.App exposing (program)
import Html.Attributes exposing (value, class, placeholder, type')
import Html.Events exposing (onInput, onSubmit, onClick)
import Random

main = program
  { init = init 
  , view = view 
  , update = update 
  , subscriptions = subscriptions 
  }

init : (Model, Cmd Msg) 
init = 
  let 
    tasks = [ (Task 1 "buy bread" False False), (Task 2 "buy milk" False True) ]
  in
    (Model tasks "", Cmd.none)


type alias Task = 
  { id : Int
  , text : String 
  , completed : Bool 
  , editing : Bool
  }


type alias Model = 
  { tasks : List Task
  , inputValue : String 
  }

type Msg 
  = UpdateInput String 
  | AddTask Int 
  | ToggleTask Int 
  | CreateTask 
  | EditTask Int
  | UpdateTask String Int

subscriptions : Model -> Sub Msg
subscriptions model = 
  Sub.none


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =  
  case msg of 
    UpdateInput val -> 
      ({ model | inputValue = val }, Cmd.none)


    AddTask genId -> 
      let 
        newTask = Task genId model.inputValue False False
      in
        ({ model | tasks = newTask::model.tasks, inputValue = "" }, Cmd.none)


    CreateTask -> 
      (model, Random.generate AddTask (Random.int 1 100000))


    EditTask id ->
      let 
        updatedTasks = 
          List.map 
            (\task -> if task.id == id then {task | editing = not task.editing} else task) 
            model.tasks
      in
        ({ model | tasks = updatedTasks }, Cmd.none)

    ToggleTask id -> 
      let 
        updatedTasks = 
          List.map 
            (\task -> if task.id == id then {task | completed = not task.completed} else task) 
            model.tasks
      in
        ({ model | tasks = updatedTasks }, Cmd.none)

    UpdateTask str id -> 
      let 
        updatedTasks = 
          List.map 
            (\task -> if task.id == id then {task | text = str } else task) 
            model.tasks
      in
        ({ model | tasks = updatedTasks }, Cmd.none)


taskInput inputValue = 
  div [ class "input-group" ] 
    [ form [ onSubmit CreateTask ] 
      [ input [ value inputValue, onInput UpdateInput, class "form-control", type' "text", placeholder "Enter task ..." ] [] ]
    , span [ class "input-group-btn" ] 
      [ button [ onClick CreateTask, class "btn btn-primary", type' "button" ] [ text "Add" ] ] 
    ]


taskItem task = 
  let 
    textClasses = if task.completed then "completed" else ""
    saveUpdates str = UpdateTask str task.id

    currentDisplay = 
      if task.editing 
      then form [ onSubmit (EditTask task.id) ] [ input [ value task.text, onInput saveUpdates ] [] ]
      else p [ class textClasses ] [ text task.text ] 
  in
    div [ class "task-item" ] 
      [ input [ type' "checkbox", onClick (ToggleTask task.id) ] []
      , currentDisplay
      , span [ class "glyphicon glyphicon-trash controls" ] [] 
      , span [ class "glyphicon glyphicon-pencil controls", onClick (EditTask task.id) ] [] 
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



