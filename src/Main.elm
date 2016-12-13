module Main exposing (..)

import Platform.Cmd as Cmd exposing (Cmd)
import Html exposing (..)
import String exposing (..)
import View exposing (view)
import Types exposing (..)
import Ports as Port exposing (playChar, correct)


{--
TODO
  - Styling
  - Animations
  - indicate overextending
  - Random chars in guess
  - add cheets (3 sec show next letter etc)
  - more sounds http://soundbible.com/free-sound-effects-1.html
  - Play word initially
--}


main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }


nextWord : Model -> GameState -> Model
nextWord model state_ =
    case state_ of
        FinishedGame collected score ->
            { guess = model.guess, state = state_ }

        Guessing g l collected score ->
            { guess = g, state = state_ }


init : ( Model, Cmd Msg )
init =
    let
        state_ =
            Types.initialState

        guess_ =
            Types.createGuess "" state_
    in
        ( { guess = guess_, state = state_ }, Cmd.none )


addChar : String -> Model -> Model
addChar ch { guess, state } =
    let
        ( g, q ) =
            guess

        g_ =
            ( String.append g ch, q )
    in
        { guess = g_, state = (Types.addGuess g_ state) }


backspace : Model -> Model
backspace { guess, state } =
    let
        ( g, q ) =
            guess

        g_ =
            String.dropRight 1 g
    in
        { guess = ( g_, q ), state = (Types.addGuess ( g_, q ) state) }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        _ =
            Debug.log ("update!")
    in
        case msg of
            AddChar ch ->
                ( addChar ch model, Cmd.none )

            Reset ->
                let
                    ( _, q ) =
                        model.guess
                in
                    ( { model | guess = ( "", q ) }, Cmd.none )

            Backspace ->
                ( backspace model, Cmd.none )

            NewWord state ->
                ( nextWord model state, Port.correct True )

            PlayChar ch ->
                ( model, Port.playChar ch )
