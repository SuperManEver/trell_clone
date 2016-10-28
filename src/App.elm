import Html exposing (..)
import Html.App as App exposing (program)
import Counter

-- MODEL
type alias AppModel = 
  { counterModel : Counter.Model }

initialModel : AppModel 
initialModel = 
  { counterModel = Counter.model }
  -- counterModels = [Counter.model]

init : (AppModel, Cmd Msg)
init = 
  (initialModel, Cmd.none)

-- MESSAGES 
type Msg = 
  -- wraps to indicate that message belongs to particular component
  -- This allows our app to route messages to the relevant components
  CounterMsg Counter.Msg  



-- UPDATE 
update : Msg -> AppModel -> ( AppModel, Cmd Msg)
update msg model =
  case msg of 
    CounterMsg subMsg ->
      let 
        ( updatedCounterModel, counterCmd ) = Counter.update subMsg model.counterModel
      in
        ( { model | counterModel = updatedCounterModel }, Cmd.map CounterMsg counterCmd )


-- SUBSCRIPTIONS
subscriptions : AppModel -> Sub Msg
subscriptions model = 
  Sub.none

-- VIEW 
view : AppModel -> Html Msg
view model = 
  div [] 
    [ App.map CounterMsg (Counter.view model.counterModel) -- ???
    , App.map CounterMsg (Counter.view model.counterModel) -- ???
    ]

main = 
  program
    { init = init
    , view = view 
    , update = update
    , subscriptions = subscriptions
    }
