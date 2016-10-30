import Html exposing (..)
import Html.App exposing (program)

main = program
  { init = init 
  , view = view 
  , update = update 
  , subscriptions = subscriptions 
  }

init : (Model, Cmd Msg) 
init = 
  (Model [], Cmd.none)

type alias Task = 
  { text : String 
  , completed : Bool 
  }

type alias Model = 
  { tasks : List Task }

type Msg = DoSomething

subscriptions : Model -> Sub Msg
subscriptions model = 
  Sub.none


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =  
  (model, Cmd.none)


view : Model -> Html Msg 
view model = 
  div [] 
    [
      input [] []
    ]



