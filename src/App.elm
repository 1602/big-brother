port module App exposing (..)

import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Decode exposing (Decoder, Value, decodeValue)
import FeatherIcons
import JsonValue exposing (JsonValue)
import View.Icons exposing (mediumIcon)
import View.App exposing (detailsBlock)
import Data.Event exposing (Event(..), TaskReport(..))
import Data.JsonDelta exposing (JsonDelta(..))
import Component.JsonViewer
import View.Task


type alias Model =
    { events : List Event
    , rays : List String
    , groupedEvents : Dict String (List Event)
    , recordingEnabled : Bool
    , groupByRay : Bool
    , selectedId : Id
    , selectedEvent : Maybe Event
    , expandedNodes : Component.JsonViewer.ExpandedNodes
    , filter : Filter
    }


type alias Id =
    String


type alias Filter =
    { succeed : Bool
    , fail : Bool
    }


init : ( Model, Cmd Msg )
init =
    { events = []
    , rays = []
    , groupedEvents = Dict.empty
    , groupByRay = True
    , recordingEnabled = True
    , selectedId = ""
    , selectedEvent = Nothing
    , expandedNodes = []
    , filter = { succeed = False, fail = True }
    }
        ! []


port event : (Value -> msg) -> Sub msg



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    if model.recordingEnabled then
        event EventReceived
    else
        Sub.none



-- UPDATE


type Msg
    = EventReceived Value
    | ToggleRecording
    | SelectEvent Id Event
    | ToggleNode (List String)


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        EventReceived event ->
            decodeValue Data.Event.decoder event
                |> Result.map2
                    (\rayId e ->
                        { model
                            | events = e :: model.events
                            , rays =
                                if List.member rayId model.rays then
                                    model.rays
                                else
                                    rayId :: model.rays
                            , groupedEvents =
                                Dict.update rayId
                                    (\events ->
                                        events
                                            |> Maybe.withDefault []
                                            |> (::) e
                                            |> Just
                                    )
                                    model.groupedEvents
                        }
                            ! []
                    )
                    (event |> decodeValue (Decode.field "rayId" Decode.string))
                |> Result.mapError (Debug.log "eventDecoder")
                |> Result.withDefault (model ! [])

        ToggleRecording ->
            { model | recordingEnabled = not model.recordingEnabled } ! []

        SelectEvent id event ->
            { model | selectedId = id, selectedEvent = Just event, expandedNodes = [] } ! []

        ToggleNode path ->
            { model
                | expandedNodes = model.expandedNodes |> Component.JsonViewer.toggle path
            }
                ! []



-- VIEW


view : Model -> Html Msg
view model =
    View.App.layout
        { sidebar =
            [ controls model.recordingEnabled
            , if model.groupByRay then
                raysStream model
              else
                eventsStream model.selectedId model.filter model.events
            ]
        , content =
            [ model.selectedEvent |> Maybe.map (\e -> viewEventDetails e model.expandedNodes) |> Maybe.withDefault (text "") ]
        }


viewEventDetails : Event -> Component.JsonViewer.ExpandedNodes -> Html Msg
viewEventDetails e expandedNodes =
    case e of
        TaskEvent _ duration _ tr ->
            View.Task.details duration tr { expandedNodes = expandedNodes, onToggle = ToggleNode }

        StateUpdate _ command state delta message ->
            div [ class "state-update" ]
                [ detailsBlock ("Message: " ++ message.name)
                    [ h4 [] [ text "payload" ]
                    , message.payload |> viewJsonValue ([ "payload" ] :: expandedNodes) [ "payload" ]
                    ]
                , detailsBlock "State Update"
                    [ h4 [] [ text "state ∆" ]
                    , viewDelta delta [ "delta" ] expandedNodes
                      -- , div [ class "json-dump" ] [ state |> Encode.encode 4 |> text ]
                    , h4 [] [ text "updated state" ]
                    , state |> viewJsonValue expandedNodes [ "state" ]
                    ]
                , detailsBlock "Command"
                    [ command |> viewJsonValue ([ "command" ] :: expandedNodes) [ "command" ]
                    ]
                ]


viewDelta : JsonDelta -> List String -> Component.JsonViewer.ExpandedNodes -> Html Msg
viewDelta delta path expandedNodes =
    let
        jsonViewer x =
            viewJsonValue expandedNodes path x
    in
        case delta of
            Data.JsonDelta.ObjectDiff props ->
                props
                    |> List.map
                        (\( key, delta ) ->
                            div []
                                [ span [ class (classifyChange delta) ] [ key ++ ":" |> text ]
                                , viewDelta delta (path ++ [ key ]) expandedNodes
                                ]
                        )
                    |> div [ class "delta" ]

            Data.JsonDelta.ValueModified before after ->
                div []
                    [ div [ class "delta--deleted" ] [ before |> jsonViewer ]
                    , div [ class "delta--added" ] [ after |> jsonViewer ]
                    ]

            Data.JsonDelta.ValueAdded jv ->
                div [ class "delta--added" ]
                    [ jv |> jsonViewer
                    ]

            Data.JsonDelta.ArrayDelta ad ->
                ad |> jsonViewer

            Data.JsonDelta.JustValue jv ->
                jv |> jsonViewer

            Data.JsonDelta.ValueDeleted jv ->
                div [ class "delta--deleted" ]
                    [ jv |> jsonViewer
                    ]

            Data.JsonDelta.NoChanges ->
                text "Nothing changed"


viewJsonValue : Component.JsonViewer.ExpandedNodes -> List String -> JsonValue -> Html Msg
viewJsonValue expandedNodes path =
    Component.JsonViewer.view { expandedNodes = expandedNodes, onToggle = ToggleNode } path


dumpValue : JsonDelta -> String
dumpValue delta =
    case delta of
        ValueAdded x ->
            x |> toString

        ValueModified _ x ->
            x |> toString

        ValueDeleted x ->
            x |> toString

        _ ->
            ""


controls : Bool -> Html Msg
controls recordingEnabled =
    div [ class "controls" ]
        [ button [ onClick ToggleRecording ]
            [ text <|
                if recordingEnabled then
                    "Pause recording"
                else
                    "Resume recording"
            ]
        ]


raysStream : Model -> Html Msg
raysStream model =
    model.rays
        |> List.reverse
        |> List.filterMap (viewRay model)
        |> List.map (\x -> div [ class "ray" ] [ x ])
        |> div [ class "events-stream list" ]


viewRay : Model -> String -> Maybe (Html Msg)
viewRay model rayId =
    model.groupedEvents
        |> Dict.get rayId
        |> Maybe.map (eventsStream model.selectedId model.filter)


eventsStream : Id -> Filter -> List Event -> Html Msg
eventsStream selectedId filter events =
    events
        |> List.reverse
        |> List.filter (applyFilter filter)
        |> List.take 200
        |> List.map (viewEvent selectedId)
        |> div [ class "events-stream list" ]


applyFilter : Filter -> Event -> Bool
applyFilter filter event =
    case event of
        TaskEvent _ _ isSuccess task ->
            case task of
                SucceedTask _ ->
                    filter.succeed

                FailTask _ ->
                    filter.fail

                _ ->
                    True

        StateUpdate _ _ _ _ _ ->
            True


viewEvent : Id -> Event -> Html Msg
viewEvent selectedId e =
    case e of
        Data.Event.TaskEvent id duration isSuccess task ->
            div
                [ classList
                    [ ( "event-container", True )
                    , ( "list__item", True )
                    , ( "list__item--success", isSuccess )
                    , ( "list__item--failure", not isSuccess )
                    , ( "list__item--selected", id == selectedId )
                    ]
                , onClick <| SelectEvent id e
                ]
                --[ duration |> toString |> text
                [ View.Task.preview task
                  --, span [] [ text (toString duration) ]
                ]

        Data.Event.StateUpdate id _ state diff msg ->
            div
                [ classList
                    [ ( "event-container", True )
                    , ( "list__item", True )
                    , ( "task-report", True )
                    , ( "list__item--neutral", True )
                    , ( "list__item--selected", id == selectedId )
                    ]
                , onClick <| SelectEvent id e
                ]
                [ FeatherIcons.activity |> mediumIcon
                , span [] [ text msg.name ]
                , case diff of
                    Data.JsonDelta.ObjectDiff props ->
                        props
                            |> List.map (\( key, delta ) -> span [ class (classifyChange delta) ] [ text key ])
                            |> span [ class "delta" ]

                    _ ->
                        text ""
                ]


classifyChange : JsonDelta -> String
classifyChange delta =
    case delta of
        ValueAdded _ ->
            "delta__key delta__key--added"

        ValueModified _ _ ->
            "delta__key delta__key--modified"

        ValueDeleted _ ->
            "delta__key delta__key--deleted"

        _ ->
            "delta__key"
