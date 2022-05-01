module Main exposing (..)

import Array exposing (..)
import Browser
import Browser.Navigation exposing (Key)
import Css exposing (..)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Html.Styled.Events exposing (onCheck, onClick, onInput)
import Http
import Iso8601
import Json.Decode as JD
import Json.Encode as JE
import Parser
import Task
import Time
import Url exposing (Url)


type alias Flags =
    {}


type Model
    = Loading { zone : Maybe Time.Zone, clocks : Maybe (Array Clock), currTime : Maybe Time.Posix }
    | Loaded { zone : Time.Zone, clocks : Array Clock, currTime : Time.Posix }
    | Error String


tryFinishLoading : Model -> Model
tryFinishLoading model =
    case model of
        Loading loading ->
            case ( loading.zone, loading.clocks, loading.currTime ) of
                ( Just zone, Just clocks, Just currTime ) ->
                    Loaded { zone = zone, clocks = clocks, currTime = currTime }

                _ ->
                    model

        _ ->
            model


updateZone : Time.Zone -> Model -> Model
updateZone zone model =
    tryFinishLoading <|
        case model of
            Loading loading ->
                Loading { loading | zone = Just zone }

            Loaded loaded ->
                Loaded { loaded | zone = zone }

            Error err ->
                Error err


setClocks clocks =
    updateClocks (always clocks)


updateClocks : (Maybe (Array Clock) -> Array Clock) -> Model -> Model
updateClocks f model =
    tryFinishLoading <|
        case model of
            Loading loading ->
                Loading { loading | clocks = Just (f loading.clocks) }

            Loaded loaded ->
                Loaded { loaded | clocks = f (Just loaded.clocks) }

            Error err ->
                Error err


updateCurrTime : Time.Posix -> Model -> Model
updateCurrTime currTime model =
    tryFinishLoading <|
        case model of
            Loading loading ->
                Loading { loading | currTime = Just currTime }

            Loaded loaded ->
                Loaded { loaded | currTime = currTime }

            Error err ->
                Error err


type alias Clock =
    { name : String
    , laps : List Time.Posix
    , explicitTime : Maybe String
    }


type Msg
    = DoNothing
    | LoadedHttpClocks (Result Http.Error (Array Clock))
    | LoadedZone Time.Zone
    | NewCurrTime Time.Posix
    | LogNewReference Int
    | SetExplicitTime Int (Maybe String)


main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = always DoNothing
        , onUrlChange = always DoNothing
        }


url =
    "https://json.extendsclass.com/bin/e3f8eb195b58"


init : Flags -> Url -> Key -> ( Model, Cmd Msg )
init _ _ _ =
    ( Loading { zone = Nothing, clocks = Nothing, currTime = Nothing }
    , Cmd.batch
        [ Http.get
            { url = url
            , expect = Http.expectJson LoadedHttpClocks (JD.array decodeClock)
            }
        , Task.perform LoadedZone Time.here
        ]
    )


view : Model -> Browser.Document Msg
view model =
    { title = "Welcome to THE CLOCK"
    , body =
        List.map toUnstyled
            [ h2 [ css [ textAlign center ] ] [ text "Welcome to THE CLOCK" ]
            , p []
                [ text "[12:17 AM] Yeah, so everytime you make a star wars reference, we have to reset the clock"
                , br [] []
                , text "[12:17 AM] talking about the clock is exempt of course"
                , br [] []
                , text "[12:17 AM] and references have to be spaced out reasonably, can't log fifty references for our Plinkett discussion"
                ]
            , case model of
                Loading _ ->
                    text "Loading clock info..."

                Error str ->
                    p [] [ text "An error occurred:", br [] [], text str ]

                Loaded { zone, clocks, currTime } ->
                    div [] <| Array.toList <| Array.indexedMap (viewClock zone currTime) clocks
            ]
    }


viewClock : Time.Zone -> Time.Posix -> Int -> Clock -> Html Msg
viewClock zone currTime idx { name, laps, explicitTime } =
    let
        toRow begin end =
            tr []
                [ td [ css [ borderRight3 (px 1) solid (rgb 0 0 0), paddingRight (ch 1) ] ] [ text <| dateString zone begin ]
                , td [ css [ paddingLeft (ch 1) ] ] [ text <| diffString (Time.posixToMillis end - Time.posixToMillis begin) ]
                ]

        toTable begins ends =
            Html.Styled.table
                [ css
                    [ whiteSpace Css.pre
                    , fontFamily monospace
                    , textAlign left
                    , fontSize (Css.em 1.5)
                    ]
                ]
                ([ tr [] [ th [] [ text "Start Date" ], th [] [ text "Duration" ] ] ]
                    ++ List.map2 toRow begins ends
                )
    in
    div []
        [ button [ onClick <| LogNewReference idx, css [ Css.marginBottom (Css.em 1) ] ] [ text "Reset THE CLOCK!" ]
        , br [] []
        , input
            [ id "toggleExplicitTime"
            , onCheck <|
                \b ->
                    SetExplicitTime idx <|
                        if b then
                            Just ""

                        else
                            Nothing
            , type_ "checkbox"
            ]
            []
        , label [ for "toggleExplicitTime" ] [ text "Use a custom time instead: " ]
        , input
            [ type_ "text"
            , onInput <| SetExplicitTime idx << Just
            , Html.Styled.Attributes.disabled (explicitTime == Nothing)
            , Html.Styled.Attributes.placeholder <|
                case explicitTime of
                    Nothing ->
                        "Disabled."

                    Just _ ->
                        "Input a valid ISO8601 time."
            , Html.Styled.Attributes.value <|
                Maybe.withDefault "" explicitTime
            , css <|
                case explicitTime of
                    Nothing ->
                        []

                    Just str ->
                        case Iso8601.toTime str of
                            Ok _ ->
                                []

                            Err _ ->
                                [ Css.borderColor (hex "f00") ]
            ]
            []
        , h3 [] [ text "Lap times:" ]
        , if List.length laps == 0 then
            text "No laps found."

          else
            toTable laps (currTime :: laps)
        ]


dateString : Time.Zone -> Time.Posix -> String
dateString zone posix =
    let
        yyyyy =
            padSpace 5 <| Time.toYear zone posix

        mmm =
            monthToThree <| Time.toMonth zone posix

        dd =
            padZero 2 <| Time.toDay zone posix

        hh =
            padZero 2 <| Time.toHour zone posix

        mm =
            padZero 2 <| Time.toMinute zone posix

        ss =
            padZero 2 <| Time.toSecond zone posix

        sss =
            padZero 3 <| Time.toMillis zone posix
    in
    String.join " " [ yyyyy, mmm, dd, String.join ":" [ hh, mm, ss ++ "." ++ sss ] ]


diffString : Int -> String
diffString millis =
    let
        secs =
            millis // 1000

        dd =
            padSpace 5 <| secs // 86400

        hh =
            padSpace 2 <| modBy 86400 secs // 3600

        mm =
            padSpace 2 <| modBy 3600 secs // 60

        ss =
            padSpace 2 <| modBy 60 secs

        sss =
            padZero 3 <| modBy 1000 millis
    in
    String.concat [ dd, " days, ", hh, " hours, ", mm, " minutes, ", ss ++ "." ++ sss, " seconds" ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        DoNothing ->
            ( model, Cmd.none )

        LoadedZone zone ->
            ( updateZone zone model, Cmd.none )

        LoadedHttpClocks resNewClocks ->
            case resNewClocks of
                Err err ->
                    ( Error (showHttpError err), Cmd.none )

                Ok clocks ->
                    ( setClocks clocks model, Cmd.none )

        NewCurrTime posix ->
            ( updateCurrTime posix model, Cmd.none )

        SetExplicitTime idx newExplicitTime ->
            case model of
                Loaded loaded ->
                    case Array.get idx loaded.clocks of
                        Nothing ->
                            ( model, Cmd.none )

                        Just targetClock ->
                            let
                                newClock =
                                    { targetClock | explicitTime = newExplicitTime }
                            in
                            ( Loaded { loaded | clocks = Array.set idx newClock loaded.clocks }
                            , Cmd.none
                            )

                _ ->
                    ( model, Cmd.none )

        LogNewReference idx ->
            case model of
                Loaded loaded ->
                    case Array.get idx loaded.clocks of
                        Nothing ->
                            ( model, Cmd.none )

                        Just targetClock ->
                            let
                                newLapTime : Result (List Parser.DeadEnd) Time.Posix
                                newLapTime =
                                    case targetClock.explicitTime of
                                        Nothing ->
                                            Ok loaded.currTime

                                        Just explicitTime ->
                                            Iso8601.toTime explicitTime
                            in
                            case newLapTime of
                                Err error ->
                                    ( model, Cmd.none )

                                Ok posix ->
                                    let
                                        newClock =
                                            let
                                                eliminateDuplicates times =
                                                    case times of
                                                        [] ->
                                                            []

                                                        x :: rest ->
                                                            case eliminateDuplicates rest of
                                                                [] ->
                                                                    x :: []

                                                                y :: restrest ->
                                                                    if abs (Time.posixToMillis x - Time.posixToMillis y) < 60000 then
                                                                        y :: restrest

                                                                    else
                                                                        x :: y :: restrest

                                                newLaps =
                                                    eliminateDuplicates <|
                                                        List.reverse <|
                                                            List.sortBy Time.posixToMillis (posix :: targetClock.laps)
                                            in
                                            { targetClock | laps = newLaps }

                                        newClocks =
                                            Array.set idx newClock loaded.clocks
                                    in
                                    ( Loaded { loaded | clocks = newClocks }
                                    , Http.request
                                        { url = url
                                        , method = "PUT"
                                        , headers = []
                                        , body = Http.jsonBody <| JE.array encodeClock newClocks
                                        , expect = Http.expectWhatever (always DoNothing)
                                        , timeout = Nothing
                                        , tracker = Nothing
                                        }
                                    )

                _ ->
                    ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every 0.139 NewCurrTime


decodeClock : JD.Decoder Clock
decodeClock =
    JD.map2
        (\name laps -> Clock name laps Nothing)
        (JD.field "name" JD.string)
        (JD.field "laps" (JD.list (JD.map Time.millisToPosix JD.int)))


encodeClock : Clock -> JE.Value
encodeClock clock =
    JE.object
        [ ( "name", JE.string clock.name )
        , ( "laps", JE.list (JE.int << Time.posixToMillis) clock.laps )
        ]


showHttpError : Http.Error -> String
showHttpError err =
    case err of
        Http.BadUrl str ->
            "Invalid url: " ++ str

        Http.Timeout ->
            "Request timed out."

        Http.NetworkError ->
            "A network error occurred."

        Http.BadStatus code ->
            "Bad HTTP status received: " ++ String.fromInt code

        Http.BadBody str ->
            "Response was malformed: " ++ str


monthToThree : Time.Month -> String
monthToThree month =
    case month of
        Time.Jan ->
            "Jan"

        Time.Feb ->
            "Feb"

        Time.Mar ->
            "Mar"

        Time.Apr ->
            "Apr"

        Time.May ->
            "May"

        Time.Jun ->
            "Jun"

        Time.Jul ->
            "Jul"

        Time.Aug ->
            "Aug"

        Time.Sep ->
            "Sep"

        Time.Oct ->
            "Oct"

        Time.Nov ->
            "Nov"

        Time.Dec ->
            "Dec"


padZero n =
    String.padLeft n '0' << String.fromInt


padSpace n =
    String.padLeft n ' ' << String.fromInt
