port module App exposing (..)

import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Decode exposing (Decoder, Value, decodeValue)
import FeatherIcons
import JsonValue exposing (JsonValue)
import View.Icons exposing (mediumIcon, smallIcon)
import View.App exposing (detailsBlock)
import Data.Event exposing (Event(..), TaskReport(..))
import Data.JsonDiff exposing (JsonDiff(..))
import Component.JsonViewer
import View.Task
import View.JsonDiff
import Request.Application
import Http


type alias Model =
    { events : List Event
    , applicationUrl : String
    , rays : List String
    , groupedEvents : Dict String (List Event)
    , recordingEnabled : Bool
    , paused : Bool
    , isConnected : Bool
    , groupByRay : Bool
    , selectedId : Id
    , selectedEvent : Maybe Event
    , selectedStartId : Id
    , selectedStartEvent : Maybe Event
    , expandedNodes : Component.JsonViewer.ExpandedNodes
    , filter : Filter
    }


type alias Id =
    String


type alias Filter =
    { succeed : Bool
    , fail : Bool
    }


init : { applicationUrl : String } -> ( Model, Cmd Msg )
init { applicationUrl } =
    { events = []
    , applicationUrl = applicationUrl
    , rays = []
    , groupedEvents = Dict.empty
    , groupByRay = True
    , recordingEnabled = True
    , paused = False
    , isConnected = False
    , selectedId = ""
    , selectedEvent = Nothing
    , selectedStartId = ""
    , selectedStartEvent = Nothing
    , expandedNodes = []
    , filter = { succeed = False, fail = True }
    }
        ! []


port event : (Value -> msg) -> Sub msg


port connected : (Bool -> msg) -> Sub msg


port setPrevState : Value -> Cmd msg



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ if model.recordingEnabled && model.isConnected then
            event EventReceived
          else
            Sub.none
        , connected SetConnectionStatus
        ]



-- UPDATE


type Msg
    = Nevermind (Result Http.Error ())
    | EventReceived Value
    | SetConnectionStatus Bool
    | ToggleRecording
    | TogglePause
    | DiscardPostponedResume
    | Resume
    | Continue
    | SelectEvent Id Event
    | PurgeEvents
    | ToggleNode (List String)


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        Nevermind _ ->
            model ! []

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

        SetConnectionStatus isConnected ->
            { model | isConnected = isConnected, selectedStartEvent = Nothing, selectedStartEvent = Nothing }
                ! [ if isConnected then
                        model.applicationUrl
                            |> Request.Application.start model.paused
                                (model.selectedStartEvent
                                    |> Maybe.andThen
                                        (\e ->
                                            case e of
                                                StateUpdate _ action state _ _ ->
                                                    Just ( state, action )

                                                _ ->
                                                    Nothing
                                        )
                                )
                            |> Http.send Nevermind
                    else
                        Cmd.none
                  ]

        ToggleRecording ->
            { model
                | recordingEnabled = not model.recordingEnabled
            }
                ! []

        DiscardPostponedResume ->
            { model
                | selectedStartEvent = Nothing
                , selectedStartId = ""
            }
                ! []

        Resume ->
            { model
                | paused = False
                , events = removeEventsAfter model.selectedId model.events
                , rays = removeRaysAfter model.selectedId model.rays model.groupedEvents
                , selectedStartEvent =
                    if model.isConnected then
                        Nothing
                    else
                        model.selectedEvent
                , selectedStartId =
                    if model.isConnected then
                        ""
                    else
                        model.selectedEvent
                            |> Maybe.andThen
                                (\e ->
                                    case e of
                                        StateUpdate id _ _ _ _ ->
                                            Just id

                                        _ ->
                                            Nothing
                                )
                            |> Maybe.withDefault ""
            }
                ! if model.isConnected then
                    if model.paused then
                        [ model.applicationUrl
                            |> Request.Application.resume
                                (model.selectedEvent
                                    |> Maybe.andThen
                                        (\e ->
                                            case e of
                                                StateUpdate _ action state _ _ ->
                                                    Just ( state, action )

                                                _ ->
                                                    Nothing
                                        )
                                )
                            |> Http.send Nevermind
                        , model.selectedEvent
                            |> Maybe.andThen
                                (\e ->
                                    case e of
                                        StateUpdate _ _ state _ _ ->
                                            state
                                                |> JsonValue.encode
                                                |> setPrevState
                                                |> Just

                                        _ ->
                                            Nothing
                                )
                            |> Maybe.withDefault Cmd.none
                        ]
                    else
                        []
                  else
                    -- TODO handle posponed re-connection (starting from desired node)
                    []

        TogglePause ->
            { model
                | paused = not model.paused
            }
                ! if model.isConnected then
                    if model.paused then
                        [ model.applicationUrl
                            |> Request.Application.resume Nothing
                            |> Http.send Nevermind
                        ]
                    else
                        [ model.applicationUrl
                            |> Request.Application.pause
                            |> Http.send Nevermind
                        ]
                  else
                    []

        Continue ->
            model
                ! [ model.applicationUrl
                        |> Request.Application.continue
                        |> Http.send Nevermind
                  ]

        SelectEvent id event ->
            { model
                | selectedId = id
                , selectedEvent = Just event
                , expandedNodes = []
            }
                ! []

        PurgeEvents ->
            { model
                | events = []
                , selectedEvent = Nothing
                , rays = []
                , groupedEvents = Dict.empty
            }
                ! []

        ToggleNode path ->
            { model
                | expandedNodes = model.expandedNodes |> Component.JsonViewer.toggle path
            }
                ! []


removeEventsAfter : Id -> List Event -> List Event
removeEventsAfter id list =
    list
        |> List.reverse
        |> List.foldl
            (\item ( result, skip ) ->
                if skip then
                    ( result, True )
                else
                    ( item :: result, isStateUpdateWithId id item )
            )
            ( [], False )
        |> (\( x, _ ) -> x)


removeRaysAfter : Id -> List String -> Dict String (List Event) -> List String
removeRaysAfter id rays groupedEvents =
    let
        containsEvent : Maybe (List Event) -> Bool
        containsEvent events =
            events
                |> Maybe.map (List.any (isStateUpdateWithId id))
                |> Maybe.withDefault False
    in
        rays
            |> List.reverse
            |> List.foldl
                (\rayId ( result, skip ) ->
                    if skip then
                        ( result, True )
                    else
                        ( rayId :: result, groupedEvents |> Dict.get rayId |> containsEvent )
                )
                ( [], False )
            |> (\( x, _ ) -> x)


isStateUpdateWithId : Id -> Event -> Bool
isStateUpdateWithId id event =
    case event of
        StateUpdate x _ _ _ _ ->
            x == id

        _ ->
            False



-- VIEW


view : Model -> Html Msg
view model =
    View.App.layout
        { sidebar =
            [ controls model
            , div [ class "controls" ]
                [ span
                    [ class "icon-button"
                    , onClick TogglePause
                    ]
                    [ smallIcon <|
                        if model.paused then
                            FeatherIcons.playCircle
                        else
                            FeatherIcons.pauseCircle
                    ]
                , span
                    [ class "icon-button"
                    , onClick Continue
                    ]
                    [ smallIcon <| FeatherIcons.cornerRightDown ]
                ]
            , div [ class "scrollable" ]
                [ if model.groupByRay then
                    raysStream model
                  else
                    eventsStream model.selectedId model.selectedStartId model.filter model.paused model.isConnected model.events
                ]
            ]
        , content =
            [ model.selectedEvent |> Maybe.map (\e -> viewEventDetails e model.expandedNodes) |> Maybe.withDefault (text "") ]
        }


viewEventDetails : Event -> Component.JsonViewer.ExpandedNodes -> Html Msg
viewEventDetails e expandedNodes =
    case e of
        TaskEvent _ duration _ tr ->
            View.Task.details duration tr { expandedNodes = expandedNodes, onToggle = ToggleNode }

        StateUpdate _ command state diff message ->
            div [ class "state-update" ]
                [ detailsBlock ("Message: " ++ message.name)
                    [ h4 [] [ text "payload" ]
                    , message.payload |> viewJsonValue ([ "payload" ] :: expandedNodes) [ "payload" ]
                    ]
                , detailsBlock "State Update"
                    [ h4 [] [ text "state ∆" ]
                    , diff |> View.JsonDiff.view [ "diff" ] { expandedNodes = expandedNodes, onToggle = ToggleNode }
                    , h4 [] [ text "updated state" ]
                    , state |> viewJsonValue expandedNodes [ "state" ]
                    ]
                , detailsBlock "Command"
                    [ command |> viewJsonValue ([ "command" ] :: expandedNodes) [ "command" ]
                    ]
                ]


viewJsonValue : Component.JsonViewer.ExpandedNodes -> List String -> JsonValue -> Html Msg
viewJsonValue expandedNodes path =
    Component.JsonViewer.view { expandedNodes = expandedNodes, onToggle = ToggleNode } path


dumpValue : JsonDiff -> String
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


controls : Model -> Html Msg
controls model =
    div [ class "controls" ] <|
        if model.isConnected then
            [ span
                [ class "icon-button"
                , onClick ToggleRecording
                , title <|
                    if model.recordingEnabled then
                        "Pause recording"
                    else
                        "Resume recording"
                ]
                [ smallIcon <|
                    if model.recordingEnabled then
                        FeatherIcons.mic
                    else
                        FeatherIcons.micOff
                ]
            , if List.isEmpty model.events then
                text ""
              else
                --button [ class "button button--outlined-secondary", onClick <| PurgeEvents ] [ FeatherIcons.trash2 |> smallIcon, text "Purge log" ]
                span [ class "icon-button", onClick <| PurgeEvents, title "Purge log" ] [ FeatherIcons.trash2 |> smallIcon ]
            , span
                [ class "icon-button"
                ]
                [ FeatherIcons.filter |> smallIcon
                ]
            ]
        else
            [ View.App.error "Disconnected from localhost:8989 (trying to reconnect automatically)"
            ]


raysStream : Model -> Html Msg
raysStream model =
    model.rays
        |> List.reverse
        |> List.filterMap (viewRay model)
        |> List.map (\x -> div [ class "ray" ] [ x ])
        |> div []


viewRay : Model -> String -> Maybe (Html Msg)
viewRay model rayId =
    model.groupedEvents
        |> Dict.get rayId
        |> Maybe.map (eventsStream model.selectedId model.selectedStartId model.filter model.paused model.isConnected)


eventsStream : Id -> Id -> Filter -> Bool -> Bool -> List Event -> Html Msg
eventsStream selectedId selectedStartId filter paused isConnected events =
    events
        |> List.reverse
        |> List.filter (applyFilter filter)
        |> List.take 200
        |> List.map (viewEvent selectedId selectedStartId paused isConnected)
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


viewEvent : Id -> Id -> Bool -> Bool -> Event -> Html Msg
viewEvent selectedId selectedStartId paused isConnected e =
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
                [ View.Icons.gitCommit
                , span
                    [ class "spread" ]
                    [ span [] [ text msg.name ]
                    , case diff of
                        Data.JsonDiff.ObjectDiff props ->
                            props
                                |> List.map (\( key, diff ) -> span [ class (View.JsonDiff.classifyChange diff) ] [ text key ])
                                |> span [ class "delta" ]

                        _ ->
                            text ""
                    ]
                , if (paused || not isConnected) && id == selectedId then
                    if id == selectedStartId then
                        span
                            [ class "icon-button", onClick DiscardPostponedResume ]
                            [ smallIcon FeatherIcons.flag ]
                    else
                        span
                            [ class "icon-button", onClick Resume ]
                            [ smallIcon FeatherIcons.playCircle ]
                  else
                    text ""
                ]
