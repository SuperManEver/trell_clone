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
    tasks = [ (Task 1 "buy bread" False False), (Task 2 "buy milk" False False) ]
  in
    (Model tasks "" All, Cmd.none)


type alias Task = 
  { id : Int
  , text : String 
  , completed : Bool 
  , editing : Bool
  }


type alias Model = 
  { tasks : List Task
  , inputValue : String 
  , filter : FilterOptions
  }

type FilterOptions = All | Active | Completed

type Msg 
  = UpdateInput String 
  | AddTask Int 
  | ToggleTask Int 
  | CreateTask 
  | EditTask Int
  | UpdateTask String Int
  | RemoveTask Int
  | FilterTasks FilterOptions


subscriptions : Model -> Sub Msg
subscriptions model = 
  Sub.none

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =  
  let
    updateTasks diff id = 
      List.map 
        (\task -> if task.id == id then (diff task) else task) 
        model.tasks
  in 
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
          xs = updateTasks (\ task -> {task | editing = not task.editing} ) id
        in
          ({ model | tasks = xs }, Cmd.none)

      ToggleTask id -> 
        let 
          xs = updateTasks (\ task -> {task | completed = not task.completed}) id 
        in 
          ({ model | tasks = xs }, Cmd.none)

      UpdateTask str id -> 
        let 
          xs = updateTasks (\ task -> {task | text = str}) id
        in
          ({ model | tasks = xs }, Cmd.none)

      RemoveTask id -> 
        let 
          xs = List.filter (\task -> task.id /= id) model.tasks
        in
          ({ model | tasks = xs }, Cmd.none)

      FilterTasks val -> 
        ({ model | filter = val}, Cmd.none)

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
      , span [ class "glyphicon glyphicon-trash controls", onClick (RemoveTask task.id) ] [] 
      , span [ class "glyphicon glyphicon-pencil controls", onClick (EditTask task.id) ] [] 
      ]


filterControls : Html Msg 
filterControls = 
  div [ class "btn-group btn-group-xs" ] 
    [ button [ type' "button", class "btn btn-default", onClick (FilterTasks All) ] [ text "All" ]
    , button [ type' "button", class "btn btn-default", onClick (FilterTasks Active)  ] [ text "Active" ]
    , button [ type' "button", class "btn btn-default", onClick (FilterTasks Completed)  ] [ text "Completed" ]
    ]

view : Model -> Html Msg 
view {inputValue, tasks, filter} = 
  let 
    tasksList = 
      case filter of 
        All -> 
          List.map (\ task -> taskItem task ) tasks

        Active -> 
          tasks 
            |> List.filter (\ task -> not task.completed )
            |> List.map (\ task -> taskItem task ) 

        Completed -> 
          tasks 
            |> List.filter (\ task -> task.completed )
            |> List.map (\ task -> taskItem task ) 

  in 
    div [ class "tasks-container" ]  
      [ taskInput inputValue
      , div [ class "tasks-list" ] tasksList
      , filterControls 
      ]



