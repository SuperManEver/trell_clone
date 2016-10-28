import Html exposing (..)
import Html.App as App exposing (program)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Counter
import List
import Random

-- MODEL
type alias AppModel = 
  { counterModels : List Counter.Model }

initialModel : AppModel 
initialModel = 
  { counterModels = [{ id = 1, value = 2}, { id = 1, value = 2}] }
  -- counterModels = [Counter.model]

init : (AppModel, Cmd Msg)
init = 
  (initialModel, Cmd.none)

-- MESSAGES 
-- wraps to indicate that message belongs to particular component
-- This allows our app to route messages to the relevant components
type Msg 
  = CounterMsg Counter.Msg  
  | AddCounter Int
  | CreateCounter
  | RemoveCounter


-- UPDATE 
update : Msg -> AppModel -> ( AppModel, Cmd Msg)
update msg model =
  case msg of 
    AddCounter id ->
      let 
        counters = (Counter.Model id 0)::model.counterModels
      in
        ({ model | counterModels = counters }, Cmd.none)

    CreateCounter ->
      (model, Random.generate AddCounter (Random.int 1 100000))

    RemoveCounter -> 
      let 
        shift     = List.drop 1
        counters  = shift model.counterModels
      in
        ({ model | counterModels = counters }, Cmd.none)

    CounterMsg subMsg -> 
      case subMsg of 
        Counter.Increment id -> 
          let 
            counterModel = List.head (List.filter (\ c -> c.id == id ) model.counterModels)
            ( updatedCounterModel, counterCmd ) = Counter.update subMsg counterModel
            counters = List.map (\ c -> if c.id == id then updatedCounterModel else c ) model.counterModels
          in
            ( { model | counterModels = counters }, Cmd.map CounterMsg counterCmd )

        Counter.Decrement id -> 
          (model, Cmd.none)
      {--          
        

      --}


-- SUBSCRIPTIONS
subscriptions : AppModel -> Sub Msg
subscriptions model = 
  Sub.none
  

-- VIEW 
view : AppModel -> Html Msg
view model = 
  let 
    counters = 
      List.map 
        (\ counterModel -> App.map CounterMsg (Counter.view counterModel)) 
        model.counterModels
  in
    div [] 
      [ button [ onClick CreateCounter ] [ text "Add Counter" ]
      , button [ onClick RemoveCounter ] [ text "Remove Counter" ]
      , div [ class "counters-holder" ] counters
      ] 
    

main = 
  program
    { init = init
    , view = view 
    , update = update
    , subscriptions = subscriptions
    }
