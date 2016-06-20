module Main exposing (..)

import String
import Html exposing (..)
import Html.App as Html
import Html.Attributes exposing (..)
import Html.Events exposing (on, onInput, onClick)
import BlackScholes exposing (..)


main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias RawInputs =
    { s : String
    , x : String
    , r : String
    , t : String
    , v : String
    }


type alias ParsedInput = Result.Result String Float


type alias ParsedInputs =
    { s : ParsedInput
    , x : ParsedInput
    , r : ParsedInput
    , t : ParsedInput
    , v : ParsedInput
    }


type alias Calculation =
    { callPrice : Float
    , putPrice : Float
    }


type alias Model =
    { rawInputs : RawInputs
    , parsedInputs : ParsedInputs
    , calculation : Maybe Calculation
    } 


parseInputs : RawInputs -> ParsedInputs
parseInputs rawInputs =
    { s = String.toFloat rawInputs.s
    , x = String.toFloat rawInputs.x
    , r = String.toFloat rawInputs.r
    , t = String.toFloat rawInputs.t
    , v = String.toFloat rawInputs.v
    }


isValidInput : ParsedInputs -> Bool
isValidInput parsedInputs =
    List.any (\x -> (Result.toMaybe x) == Maybe.Nothing)
        [ parsedInputs.s
        , parsedInputs.x
        , parsedInputs.r
        , parsedInputs.t
        , parsedInputs.v
        ] 
        |> not


calculate : ParsedInputs -> Maybe Calculation
calculate parsedInputs =
    if isValidInput parsedInputs then
        let
            pi = parsedInputs
            s' = pi.s |> Result.withDefault 0
            x' = pi.x |> Result.withDefault 0
            r' = pi.r |> Result.withDefault 0
            t' = pi.t |> Result.withDefault 0
            v' = pi.v |> Result.withDefault 0

        in
            Just 
                { callPrice = callPrice s' x' r' t' v'
                , putPrice = putPrice s' x' r' t' v'
                }
    else
        Maybe.Nothing


init : ( Model, Cmd Msg )
init =
    let
        initialRawInputs =
            RawInputs "49" "50" "0.0255" "2" "0.2"

        initialParsedInputs = 
            parseInputs initialRawInputs

        initialCalculation =
            calculate initialParsedInputs

    in
        ( Model initialRawInputs initialParsedInputs initialCalculation, Cmd.none )


-- UPDATE


type Msg
    = UpdateS String
    | UpdateX String
    | UpdateR String
    | UpdateT String
    | UpdateV String
    | Calculate


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        rawInputs = model.rawInputs
    in
        case msg of
            UpdateS val ->
                let
                    rawInputs' = { rawInputs | s = val }
                    parsedInputs' = parseInputs rawInputs'
                in
                    ( { model | rawInputs = rawInputs', parsedInputs = parsedInputs' }, Cmd.none)
                
            UpdateX val ->
                let
                    rawInputs' = { rawInputs | x = val }
                    parsedInputs' = parseInputs rawInputs'
                in
                    ( { model | rawInputs = rawInputs', parsedInputs = parsedInputs' }, Cmd.none)
                
            UpdateR val ->
                let
                    rawInputs' = { rawInputs | r = val }
                    parsedInputs' = parseInputs rawInputs'
                in
                    ( { model | rawInputs = rawInputs', parsedInputs = parsedInputs' }, Cmd.none)
                
            UpdateT val ->
                let
                    rawInputs' = { rawInputs | t = val }
                    parsedInputs' = parseInputs rawInputs'
                in
                    ( { model | rawInputs = rawInputs', parsedInputs = parsedInputs' }, Cmd.none)
                
            UpdateV val ->
                let
                    rawInputs' = { rawInputs | v = val }
                    parsedInputs' = parseInputs rawInputs'
                in
                    ( { model | rawInputs = rawInputs', parsedInputs = parsedInputs' }, Cmd.none)
                

            Calculate ->
                ( { model | calculation =  calculate model.parsedInputs }, Cmd.none )


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    let
        rawInputs = model.rawInputs
        parsedInputs = model.parsedInputs

    in
        div []
            [ inputView "underlying" rawInputs.s parsedInputs.s UpdateS
            , inputView "strike" rawInputs.x parsedInputs.x UpdateX
            , inputView "rate" rawInputs.r parsedInputs.r UpdateR
            , inputView "time" rawInputs.t parsedInputs.t UpdateT
            , inputView "volatility" rawInputs.v parsedInputs.v UpdateV
            , div [] 
                [ button [ onClick Calculate ] [ text "Calculate" ] ]
            , calcView model.calculation
            ]


inputView : String -> String -> ParsedInput -> (String -> a) -> Html a
inputView label' rawValue parsedValue message =
    let
        valMsg =
            case parsedValue of
                Result.Ok val ->
                    "OK"
                Result.Err msg ->
                    msg
    in
        div [] 
            [ label [] [ text label' ]
            , input [ type' "number", value rawValue, onInput message ] []
            , span [] [ text valMsg ]
            ]


calcView : Maybe Calculation -> Html Msg
calcView calcs =
    case calcs of
        Nothing ->
            div [] [ text "Enter valid inputs" ]

        Just c ->
            dl []
                [ dt [] [ text "call" ]
                , dd [] [ text (toString c.callPrice) ]
                , dt [] [ text "put" ]
                , dd [] [ text (toString c.putPrice) ]
                ]
