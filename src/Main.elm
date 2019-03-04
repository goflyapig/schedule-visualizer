module Main exposing (main)

import Browser
import Dict
import Html exposing (Html, div, h1, text, textarea)
import Html.Attributes exposing (class, style, title)
import Html.Events exposing (onInput)
import Json.Decode as D
import Parser exposing ((|.), (|=))


main : Platform.Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    String


init : () -> ( Model, Cmd msg )
init () =
    let
        json =
            """[{"description":"The Curls","start":"1:00 pm","end":"1:40 pm","location":"Green Stage"},{"description":"Melkbelly","start":"1:45 pm","end":"2:25 pm","location":"Red Stage"},{"description":"Lucy Dacus","start":"2:30 pm","end":"3:15 pm","location":"Green Stage"},{"description":"Julie Byrne","start":"2:45 pm","end":"3:30 pm","location":"Blue Stage"},{"description":"Joshua Abrams & Natural Information Society","start":"3:20 pm","end":"4:10 pm","location":"Red Stage"},{"description":"Open Mike Eagle","start":"4:00 pm","end":"4:45 pm","location":"Blue Stage"},{"description":"Saba","start":"4:15 pm","end":"5:10 pm","location":"Green Stage"},{"description":"Syd","start":"5:15 pm","end":"6:10 pm","location":"Red Stage"},{"description":"Julien Baker","start":"5:15 pm","end":"6:00 pm","location":"Blue Stage"},{"description":"Earl Sweatshirt","start":"6:15 pm","end":"7:15 pm","location":"Green Stage"},{"description":"Big Thief","start":"6:30 pm","end":"7:15 pm","location":"Blue Stage"},{"description":"Courtney Barnett","start":"7:25 pm","end":"8:25 pm","location":"Red Stage"},{"description":"Mount Kimbie","start":"7:45 pm","end":"8:30 pm","location":"Blue Stage"},{"description":"Tame Impala","start":"8:30 pm","end":"9:50 pm","location":"Green Stage"}]"""
    in
    ( json, Cmd.none )


type alias Event =
    { description : String
    , start : Time
    , end : Time
    , location : String
    }


type alias Time =
    { hour : Int
    , minute : Int
    }


type alias LocationGroup =
    { location : String
    , events : List Event
    }


eventsToLocationGroups : List Event -> List LocationGroup
eventsToLocationGroups events =
    let
        addEvent event dict =
            Dict.update event.location (updateGroup event) dict

        updateGroup event maybeGroup =
            Just (event :: Maybe.withDefault [] maybeGroup)

        locationDict =
            List.foldl addEvent Dict.empty events

        startTime locationGroup =
            locationGroup.events
                |> List.map (\e -> minutesFromMidnight e.start)
                |> List.minimum
                |> Maybe.withDefault 0
    in
    locationDict
        |> Dict.toList
        |> List.map (\kv -> { location = Tuple.first kv, events = Tuple.second kv })
        |> List.sortBy startTime


minutesFromMidnight : Time -> Int
minutesFromMidnight time =
    time.hour * 60 + time.minute



-- UPDATE


type Msg
    = SetJson String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg _ =
    case msg of
        SetJson newValue ->
            ( newValue, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div [ class "schedule-visualizer" ]
        [ h1 [] [ text "Schedule Visualizer Thing" ]
        , textarea [ class "json-input", onInput SetJson ] [ text model ]
        , viewSchedule model
        ]


viewSchedule : Model -> Html Msg
viewSchedule model =
    case D.decodeString scheduleDecoder model of
        Ok events ->
            let
                window =
                    windowForEvents events

                timeHeadingElements =
                    List.range window.startHour window.endHour
                        |> List.concatMap (\hour -> [ { hour = hour, minute = 0 }, { hour = hour, minute = 30 } ])
                        |> List.map viewTimeHeading

                viewTimeHeading time =
                    div [ class "time-heading", style "left" (timeLeftPosition window time) ]
                        [ text
                            (if time.minute == 0 then
                                timeToString time

                             else
                                ""
                            )
                        ]

                locationGroupElements =
                    eventsToLocationGroups events |> List.map viewLocationGroup

                viewLocationGroup locationGroup =
                    div [ class "location-group" ]
                        [ div [ class "location-head" ] [ text locationGroup.location ]
                        , div [ class "events" ] (locationGroup.events |> List.map viewEvent)
                        ]

                viewEvent event =
                    div
                        [ class "event"
                        , style "left" (timeLeftPosition window event.start)
                        , style "width" (timeWidth event.start event.end)
                        , title event.description
                        ]
                        [ text event.description ]
            in
            div [ class "schedule" ]
                [ div [ class "time-headings" ] timeHeadingElements
                , div [ class "location-groups" ] locationGroupElements
                ]

        Err error ->
            div [ class "json-error" ] [ text (D.errorToString error) ]


type alias Window =
    { startHour : Int
    , endHour : Int
    }


windowForEvents : List Event -> Window
windowForEvents events =
    { startHour =
        events
            |> List.map (\event -> event.start.hour)
            |> List.minimum
            |> Maybe.withDefault 0
    , endHour =
        events
            |> List.map (\event -> minutesFromMidnight event.end)
            |> List.map (\minutes -> ceiling (toFloat minutes / 60))
            |> List.maximum
            |> Maybe.withDefault 0
    }


emPerHour =
    14


minutesWidth : Int -> String
minutesWidth minutes =
    String.fromFloat (toFloat minutes * emPerHour / 60) ++ "em"


timeWidth : Time -> Time -> String
timeWidth start end =
    minutesWidth (minutesFromMidnight end - minutesFromMidnight start)


timeLeftPosition : Window -> Time -> String
timeLeftPosition window time =
    minutesWidth (minutesFromMidnight time - (window.startHour * 60))


timeToString : Time -> String
timeToString time =
    let
        amPm =
            if time.hour >= 12 then
                "pm"

            else
                "am"

        displayHour =
            if modBy 12 time.hour == 0 then
                "12"

            else
                String.fromInt (modBy 12 time.hour)
    in
    if time.minute == 0 then
        displayHour ++ amPm

    else
        displayHour ++ ":" ++ String.fromInt time.minute ++ amPm



-- DECODERS


scheduleDecoder : D.Decoder (List Event)
scheduleDecoder =
    D.list entryDecoder


entryDecoder : D.Decoder Event
entryDecoder =
    let
        validateEvent event =
            if minutesFromMidnight event.start > minutesFromMidnight event.end then
                D.fail "Event cannot end before it starts"

            else
                D.succeed event
    in
    D.map4 Event
        (D.field "description" D.string)
        (D.field "start" timeDecoder)
        (D.field "end" timeDecoder)
        (D.field "location" D.string)
        |> D.andThen validateEvent


timeDecoder : D.Decoder Time
timeDecoder =
    let
        parseTime timeString =
            case Parser.run timeParser (String.toLower timeString) of
                Ok time ->
                    D.succeed time

                Err err ->
                    D.fail
                        "Invalid time"
    in
    D.string
        |> D.andThen parseTime


timeParser : Parser.Parser Time
timeParser =
    Parser.succeed timeFromParsedComponents
        |. Parser.spaces
        |= intWithLeadingZeroesParser
        |= Parser.oneOf
            [ Parser.succeed identity
                |. Parser.symbol ":"
                |= intWithLeadingZeroesParser
            , Parser.succeed 0
            ]
        |. Parser.spaces
        |= Parser.oneOf
            [ Parser.keyword "pm" |> Parser.map (\_ -> True)
            , Parser.keyword "am" |> Parser.map (\_ -> False)
            , Parser.succeed False
            ]
        |. Parser.spaces
        |. Parser.end


intWithLeadingZeroesParser : Parser.Parser Int
intWithLeadingZeroesParser =
    Parser.oneOf
        [ Parser.succeed identity
            |. Parser.chompIf (\c -> c == '0')
            |. Parser.chompWhile (\c -> c == '0')
            |= Parser.oneOf
                [ Parser.int
                , Parser.succeed 0
                ]
        , Parser.int
        ]


timeFromParsedComponents : Int -> Int -> Bool -> Time
timeFromParsedComponents hour minute isPm =
    if isPm && hour < 12 then
        { hour = hour + 12, minute = minute }

    else
        { hour = hour, minute = minute }
