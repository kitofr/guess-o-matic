module View exposing (view)

import Html exposing (..)
import Html.Attributes as A
import Html.Events exposing (onClick)
import String exposing (..)
import Set exposing (..)
import List exposing (append)
import Types exposing (..)
import Seq exposing (..)
import Data exposing (..)
import Svg
import Svg.Attributes as SvgA


row : List (Html Msg) -> Html Msg
row =
    div [ A.class "row", rowDistance ]


rowDistance =
    A.style [ ( "margin-bottom", "10px" ), ( "margin-top", "25px" ) ]


questionArea =
    div
        [ A.style
            [ ( "width", "90%" )
            , ( "display", "block" )
            , ( "border-radius", "10px" )
            , ( "background", "white" )
            , ( "padding", "10px" )
            , ( "margin", "auto" )
            ]
        ]


container_ : List (Html Msg) -> Html Msg
container_ =
    div
        [ A.class "container text-center"
        , A.style
            [ ( "background-color", "#EEE" )
            , ( "width", "100%" )
            , ( "height", "100%" )
            ]
        ]


centerInline width =
    A.style
        [ ( "width", (toString width) ++ "%" )
        , ( "display", "inline-block" )
        , ( "margin", "auto" )
        ]


center width =
    [ ( "width", (toString width) ++ "%" )
    , ( "display", "block" )
    , ( "margin", "auto" )
    ]


stylesheet : String -> Html Msg
stylesheet href =
    node "link"
        [ A.rel "stylesheet"
        , A.href href
        ]
        []


smallButtonStyle =
    A.style
        [ ( "font-size", "16px" )
        , ( "margin-right", "5px" )
        ]


pushToBottom =
    [ A.style
        [ ( "margin-top", "100px" )
        , ( "margin-bottom", "100px" )
        ]
    ]


checkButton =
    let
        style =
            List.append (center 90) [ ( "font-size", "35px" ) ]
    in
        [ A.style style
        , A.class "btn btn-success"
        , onClick CheckWord
        ]


megaButton =
    A.style
        [ ( "width", "130px" )
        , ( "font-size", "40px" )
        ]


buttonStyle =
    A.style
        [ ( "margin-right", "10px" )
        , ( "width", "100px" )
        , ( "font-size", "20px" )
        , ( "padding", "10px" )
        ]


btn action classes what =
    button [ classes, buttonStyle, onClick action ]
        what


primaryBtn label action =
    btn action (A.class "btn btn-primary") [ text label ]


charButton c action =
    primaryBtn c (action c)


bigAndSmall : String -> String
bigAndSmall s =
    s ++ " " ++ (toLower s)


addButtons answer msg =
    List.map (\c -> charButton (String.fromChar c) msg) (uniqueChars answer)


iconButton action =
    btn action (A.class "btn glyphicon glyphicon-volume-up") []


addIconButtons answer action =
    List.map (\c -> iconButton (action (String.fromChar c))) (uniqueChars answer)


image : Guess -> String
image ( _, question ) =
    question.image


currentAnswer : Model -> String
currentAnswer { guess, state } =
    let
        ( _, question ) =
            guess
    in
        word question


hasMoreWords : Model -> Bool
hasMoreWords { guess, state } =
    let
        lst =
            wordList state
    in
        case lst of
            [] ->
                False

            otherwise ->
                True


picture : Model -> Html Msg
picture { guess, state } =
    div []
        [ img
            [ A.src (image guess)
            , A.width 300
            , A.height 300
            , A.style
                [ ( "border", "1px solid #AAA" )
                , ( "border-radius", "25px" )
                , ( "padding", "10px" )
                , ( "-webkit-box-shadow", "0 10px 6px -6px #777" )
                , ( "-moz-box-shadow", "0 10px 6px -6px #777" )
                , ( "box-shadow", "0 10px 6px -6px #777" )
                ]
            ]
            []
        ]


controlButton action icon =
    button
        [ A.class ("btn btn-warning glyphicon " ++ icon)
        , buttonStyle
        , onClick action
        ]
        []


textControls model =
    row
        [ div []
            [ controlButton Reset "glyphicon-refresh"
            , controlButton Backspace "glyphicon-erase"
            ]
        ]


disabledButton ch =
    let
        t =
            String.fromChar ch
    in
        button [ A.class "btn btn-disabled", buttonStyle, onClick Backspace ] [ text t ]


paddUpTo : List Char -> Int -> List Char
paddUpTo lst n =
    if List.length lst < n then
        paddUpTo (List.append lst [ '_' ]) n
    else
        lst


answer : Guess -> String
answer ( _, question ) =
    (word question)


showGuess : Model -> Html Msg
showGuess { guess, state } =
    let
        answer_ =
            String.toList (answer guess)

        paddTo =
            (List.length answer_)

        paddedGuess =
            paddUpTo (String.toList (Tuple.first guess)) paddTo
    in
        row
            [ div []
                (List.map disabledButton paddedGuess)
            ]


letterButtons : Model -> Html Msg
letterButtons model =
    row
        [ div []
            (addButtons (currentAnswer model) AddChar)
        ]


soundButtons : Model -> Html Msg
soundButtons model =
    row [ div [] (addIconButtons (currentAnswer model) PlayChar) ]


success : Model -> Html Msg
success model =
    row
        [ div []
            (checkAnswer model)
        ]


collectedChars : Set Char -> List (Html Msg)
collectedChars collected =
    (List.map (\c -> button [ A.class "btn btn-disabled", smallButtonStyle ] [ text c ])
        (List.map String.fromChar (Set.toList collected))
    )


checkAnswer : Model -> List (Html Msg)
checkAnswer { guess, state } =
    case state of
        FinishedGame collected score ->
            [ section [ A.style [ ( "margin-top", "35px" ) ] ]
                (collectedChars collected)
            ]

        Guessing g wordlist collected score ->
            -- TODO: Extract this to own model/view and add animations
            if correct guess then
                [ button [ A.class "btn btn-success", megaButton, onClick (NewWord state) ]
                    [ span [ A.class "glyphicon glyphicon-thumbs-up" ] [] ]
                ]
            else
                [ div [] [] ]


progress : Model -> Html Msg
progress { guess, state } =
    let
        n =
            wordList state |> List.length |> Basics.toFloat

        tot =
            alternatives |> List.length |> Basics.toFloat

        total =
            500.0

        complete =
            ((tot - n) / tot) * total

        width =
            (toString total)
    in
        div [ centerInline 90 ]
            [ Svg.svg
                [ SvgA.width "100%", SvgA.height "20", SvgA.viewBox ("0 0 " ++ width ++ " 20") ]
                [ Svg.rect [ SvgA.fill "#AAA", SvgA.x "0", SvgA.y "0", SvgA.width width, SvgA.height "20", SvgA.rx "5", SvgA.ry "5" ] []
                , Svg.rect [ SvgA.fill "#8C8", SvgA.x "0", SvgA.y "0", SvgA.width (toString complete), SvgA.height "20", SvgA.rx "5", SvgA.ry "5" ] []
                ]
            ]


quit =
    span
        [ A.style
            [ ( "float", "left" )
            , ( "margin-left", "20px" )
            ]
        ]
        [ button
            [ A.class "btn btn-warning"
            , A.style [ ( "display", "inline" ) ]
            , onClick Quit
            ]
            [ text "Quit" ]
        ]


score : Model -> Html Msg
score { guess, state } =
    let
        score =
            (currentScore state)

        fontX =
            (toString (85 - ((score // 10) * 12)))
    in
        Svg.svg
            [ SvgA.width "200", SvgA.height "200", SvgA.viewBox "0 0 200 200" ]
            [ Svg.polygon
                [ SvgA.fill "#EE9"
                , SvgA.points "100,10 40,198 190,78 10,78 160,198"
                ]
                []
            , Svg.text_
                [ SvgA.fontSize "45", SvgA.x fontX, SvgA.y "130", SvgA.fill "blue" ]
                [ Svg.text (toString score) ]
            ]


view : Model -> Html Msg
view model =
    main_ []
        [ header []
            [ stylesheet "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.5/css/bootstrap.min.css" ]
        , body []
            [ container_
                [ row
                    [ quit
                    , progress model
                    ]
                , row
                    [ questionArea
                        [ picture model
                        , div [ A.class "row" ]
                            [--score model
                             --success model
                            ]
                        , showGuess model
                        , letterButtons model
                        , soundButtons model
                        ]
                    ]
                  --, textControls model
                , div pushToBottom
                    [ button checkButton [ text "Kontrollera" ] ]
                ]
            ]
        ]
