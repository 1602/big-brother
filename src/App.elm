port module App exposing (..)

import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Decode exposing (Decoder, Value, decodeValue)
import Json.Encode as Encode
import Time exposing (Time)
import FeatherIcons
import Date
import Strftime
import JsonValue exposing (JsonValue)
import Icons


type alias Model =
    { events : List Event
    , rays : List String
    , groupedEvents : Dict String (List Event)
    , recordingEnabled : Bool
    , groupByRay : Bool
    , selectedId : Id
    , selectedEvent : Maybe Event
    , expandedNodes : List (List String)
    , filter : Filter
    }


type alias Id =
    String


type alias Filter =
    { succeed : Bool
    , fail : Bool
    }


type Event
    = StateUpdate Id JsonValue JsonValue JsonDelta Message
    | TaskEvent Id Int Bool TaskReport


type alias Message =
    { name : String
    , payload : JsonValue
    }


type TaskReport
    = HttpRequest
        { request :
            { url : String
            , method : String
            , data : Maybe JsonValue
            , headers : List ( String, String )
            }
        , response :
            Maybe
                { status : { ok : Bool, code : Int, text : String }
                , headers : List ( String, List String )
                , error : Maybe JsonValue
                , body : Maybe JsonValue
                }
        , error : Maybe JsonValue
        }
    | FailTask Value
    | SucceedTask Value
    | CurrentTime Time
    | UnknownTask Value


type JsonDelta
    = ObjectDiff (List ( String, JsonDelta ))
    | ValueAdded JsonValue
    | ValueModified JsonValue JsonValue
    | ValueDeleted JsonValue
    | NoChanges
    | ArrayDelta JsonValue
    | JustValue JsonValue


deltaDecoder : Decoder JsonDelta
deltaDecoder =
    Decode.oneOf
        [ Decode.field "_t" Decode.string
            |> Decode.andThen
                (\s ->
                    if s == "a" then
                        Decode.map ArrayDelta JsonValue.decoder
                    else
                        JsonValue.decoder |> Decode.map JustValue
                )
        , Decode.keyValuePairs (Decode.lazy (\_ -> deltaDecoder)) |> Decode.map ObjectDiff
        , Decode.list JsonValue.decoder
            |> Decode.andThen
                (\list ->
                    case list of
                        [] ->
                            NoChanges
                                |> Decode.succeed

                        [ newValue ] ->
                            ValueAdded newValue
                                |> Decode.succeed

                        [ oldValue, newValue ] ->
                            ValueModified oldValue newValue
                                |> Decode.succeed

                        [ oldValue, _, _ ] ->
                            ValueDeleted oldValue
                                |> Decode.succeed

                        _ ->
                            Decode.fail "Not a delta"
                )
        , JsonValue.decoder |> Decode.map JustValue
        ]


eventDecoder : Decoder Event
eventDecoder =
    Decode.oneOf [ stateUpdateDecoder, taskDecoder ]


stateUpdateDecoder : Decoder Event
stateUpdateDecoder =
    Decode.map5 StateUpdate
        (Decode.field "id" Decode.string)
        (Decode.field "cmd" JsonValue.decoder)
        (Decode.field "state" JsonValue.decoder)
        (Decode.field "delta" deltaDecoder)
        (Decode.field "msg"
            (Decode.map2 Message
                (Decode.field "name" Decode.string)
                (Decode.field "payload" JsonValue.decoder)
            )
        )


taskDecoder : Decoder Event
taskDecoder =
    Decode.map4 TaskEvent
        (Decode.field "id" Decode.string)
        (Decode.field "duration" Decode.int)
        (Decode.at [ "result", "result" ] (Decode.string |> Decode.map ((==) "success")))
        taskReportDecoder


taskReportDecoder : Decoder TaskReport
taskReportDecoder =
    Decode.oneOf
        [ httpRequestDecoder
        , currentTimeTaskDecoder
        , failSucceedTaskDecoder
        , Decode.value |> Decode.map UnknownTask
        ]


failSucceedTaskDecoder : Decoder TaskReport
failSucceedTaskDecoder =
    Decode.at [ "spec", "task" ] Decode.string
        |> Decode.andThen
            (\task ->
                if task == "fail" then
                    Decode.map FailTask (Decode.at [ "spec", "error" ] Decode.value)
                else if task == "succeed" then
                    Decode.map SucceedTask (Decode.at [ "spec", "data" ] Decode.value |> Decode.maybe |> Decode.map (Maybe.withDefault Encode.null))
                else
                    Decode.fail "Doesn't look like fail task"
            )


currentTimeTaskDecoder : Decoder TaskReport
currentTimeTaskDecoder =
    Decode.map CurrentTime
        (Decode.field "spec" Decode.string
            |> Decode.andThen
                (\s ->
                    if s == "time" then
                        Decode.at [ "result", "data" ] Decode.float
                    else
                        Decode.fail "Not a current time task"
                )
        )


httpRequestDecoder : Decoder TaskReport
httpRequestDecoder =
    Decode.map3 (\req res err -> HttpRequest { request = req, response = res, error = err })
        (Decode.field "spec"
            (Decode.map4 (\url method headers data -> { url = url, method = method, headers = headers, data = data })
                (Decode.field "url" Decode.string)
                (Decode.field "method" Decode.string)
                (Decode.field "headers" (Decode.keyValuePairs Decode.string))
                (Decode.field "data" JsonValue.decoder |> Decode.maybe)
            )
        )
        (Decode.at [ "result", "data" ]
            (Decode.map4 (\status headers error body -> { status = status, headers = headers, error = error, body = body })
                (Decode.field "status"
                    (Decode.map3 (\ok code text -> { ok = ok, code = code, text = text })
                        (Decode.field "ok" Decode.bool)
                        (Decode.field "code" Decode.int)
                        (Decode.field "text" Decode.string)
                    )
                )
                (Decode.field "headers" (Decode.keyValuePairs (Decode.list Decode.string)))
                (Decode.field "error" JsonValue.decoder |> Decode.maybe)
                (Decode.field "body" JsonValue.decoder |> Decode.maybe)
            )
            |> Decode.maybe
        )
        (Decode.at [ "result", "error" ] JsonValue.decoder |> Decode.maybe)


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
            decodeValue eventDecoder event
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
                | expandedNodes =
                    if List.member path model.expandedNodes then
                        model.expandedNodes |> List.filter ((/=) path)
                    else
                        path :: model.expandedNodes
            }
                ! []



-- VIEW


view : Model -> Html Msg
view model =
    div [ class "app__container" ]
        [ div [ class "app__sidebar" ]
            [ controls model.recordingEnabled
            , if model.groupByRay then
                raysStream model
              else
                eventsStream model.selectedId model.filter model.events
            ]
        , div [ class "app__content" ]
            [ model.selectedEvent |> Maybe.map (\e -> viewEventDetails e model.expandedNodes) |> Maybe.withDefault (text "") ]
        ]


viewEventDetails : Event -> List (List String) -> Html Msg
viewEventDetails e expandedNodes =
    case e of
        TaskEvent _ duration _ tr ->
            viewTaskDetails duration tr expandedNodes

        StateUpdate _ command state delta message ->
            div [ class "state-update" ]
                [ detailsBlock ("Message: " ++ message.name)
                    [ h4 [] [ text "payload" ]
                    , viewJsonValue message.payload [ "payload" ] ([ "payload" ] :: expandedNodes)
                    ]
                , detailsBlock "State Update"
                    [ h4 [] [ text "state ∆" ]
                    , viewDelta delta [ "delta" ] expandedNodes
                      -- , div [ class "json-dump" ] [ state |> Encode.encode 4 |> text ]
                    , h4 [] [ text "updated state" ]
                    , viewJsonValue state [ "state" ] expandedNodes
                    ]
                , detailsBlock "Command"
                    [ viewJsonValue command [ "command" ] ([ "command" ] :: expandedNodes)
                    ]
                ]


viewDelta : JsonDelta -> List String -> List (List String) -> Html Msg
viewDelta delta path expandedNodes =
    case delta of
        ObjectDiff props ->
            props
                |> List.map
                    (\( key, delta ) ->
                        div []
                            [ span [ class (classifyChange delta) ] [ key ++ ":" |> text ]
                            , viewDelta delta (path ++ [ key ]) expandedNodes
                            ]
                    )
                |> div [ class "delta" ]

        ValueModified before after ->
            div []
                [ div [ class "delta--deleted" ] [ viewJsonValue before path expandedNodes ]
                , div [ class "delta--added" ] [ viewJsonValue after path expandedNodes ]
                ]

        ValueAdded jv ->
            div [ class "delta--added" ]
                [ viewJsonValue jv path expandedNodes
                ]

        ArrayDelta ad ->
            viewJsonValue ad path expandedNodes

        JustValue jv ->
            viewJsonValue jv path expandedNodes

        ValueDeleted jv ->
            div [ class "delta--deleted" ]
                [ viewJsonValue jv path expandedNodes
                ]

        NoChanges ->
            text "Nothing changed"


viewJsonValue : JsonValue -> List String -> List (List String) -> Html Msg
viewJsonValue jv path expandedNodes =
    case jv of
        JsonValue.BoolValue bv ->
            span [ class "json-value json-value--bool" ]
                [ text <|
                    if bv then
                        "true"
                    else
                        "false"
                ]

        JsonValue.NumericValue nv ->
            span [ class "json-value json-value--number" ] [ nv |> toString |> text ]

        JsonValue.StringValue sv ->
            span [ class "json-value json-value--string" ] [ sv |> toString |> text ]

        JsonValue.NullValue ->
            span [ class "json-value json-value--null" ] [ text "null" ]

        JsonValue.ObjectValue props ->
            if List.member path expandedNodes then
                props
                    |> List.map
                        (\( k, v ) ->
                            div [ class "json-value json-value__object-property" ]
                                [ span [ class "json-value json-value__key" ] [ text k ]
                                , viewJsonValue v (path ++ [ k ]) expandedNodes
                                ]
                        )
                    |> div [ class "json-value json-value--expandable" ]
            else
                props
                    |> List.take 5
                    |> List.map (\( k, _ ) -> k)
                    |> String.join ", "
                    |> (\s -> span [ class "json-value json-value--collapsed", onClick <| ToggleNode path ] [ "{ " ++ s ++ "... }" |> text ])

        JsonValue.ArrayValue items ->
            if List.member path expandedNodes then
                items
                    |> List.indexedMap
                        (\index v ->
                            div []
                                [ span [ class "json-value json-value__key" ] [ toString index |> text ]
                                , viewJsonValue v (path ++ [ toString index ]) expandedNodes
                                ]
                        )
                    |> div [ class "json-value json-value--expandable" ]
            else
                span
                    [ class "json-value json-value--collapsed"
                    , onClick <| ToggleNode path
                    ]
                    [ "[ " ++ (List.length items |> toString) ++ " items... ]" |> text
                    ]


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


detailsBlock : String -> List (Html msg) -> Html msg
detailsBlock header content =
    div [ class "details-block" ]
        [ h3 [ class "details-block__header" ] [ text header ]
        , section [ class "details-block__content" ] content
        ]


viewTaskDetails : Int -> TaskReport -> List (List String) -> Html Msg
viewTaskDetails duration tr expandedNodes =
    case tr of
        HttpRequest http ->
            div [ class "http" ]
                [ detailsBlock "Request"
                    [ div [ class "task-report http-request" ]
                        [ span [ class ("method " ++ (http.request.method |> String.toLower)) ] [ http.request.method |> text ]
                        , span [ class "url" ] [ http.request.url |> text ]
                        ]
                    , h4 [] [ text "Headers" ]
                    , http.request.headers
                        |> List.map
                            (\( header, value ) ->
                                div []
                                    [ span [ class "http__header-name" ] [ text header ]
                                    , span [ class "http__header-value" ] [ value |> text ]
                                    ]
                            )
                        |> div []
                    , h4 [] [ text "Body" ]
                    , div []
                        [ http.request.data
                            |> Maybe.map (\jv -> viewJsonValue jv [] expandedNodes)
                            |> Maybe.withDefault ("Ø" |> text)
                        ]
                    ]
                , detailsBlock "Response" <|
                    case http.response of
                        Just response ->
                            [ div []
                                [ span
                                    [ classList
                                        [ ( "badge", True )
                                        , ( "badge--success", response.status.ok )
                                        , ( "badge--failure", not response.status.ok )
                                        ]
                                    ]
                                    [ response.status.code
                                        |> toString
                                        |> text
                                    ]
                                , span [] [ " " ++ response.status.text |> text ]
                                ]
                            , h4 [] [ text "Headers" ]
                            , response.headers
                                |> List.map
                                    (\( header, values ) ->
                                        div []
                                            [ span [ class "http__header-name" ] [ text header ]
                                            , span [ class "http__header-value" ] [ values |> String.join ", " |> text ]
                                            ]
                                    )
                                |> div []
                            , h4 [] [ text "Body" ]
                            , response.body |> Maybe.map (\jv -> viewJsonValue jv [] expandedNodes) |> Maybe.withDefault (text "Ø")
                            ]

                        Nothing ->
                            case http.error of
                                Just error ->
                                    [ h4 [] [ text "Error" ]
                                    , div []
                                        [ viewJsonValue error [] expandedNodes
                                        ]
                                    ]

                                Nothing ->
                                    [ text "In progress, perhaps" ]
                , detailsBlock "Duration" <|
                    [ toString duration ++ "ms" |> text
                    ]
                ]

        CurrentTime x ->
            div [ class "task-report" ]
                [ FeatherIcons.clock |> FeatherIcons.withStrokeWidth 2 |> FeatherIcons.withSize 18 |> FeatherIcons.toHtml []
                , span [] [ Strftime.format "%B %d %Y, %-I:%M:%S" (Date.fromTime x) |> text ]
                ]

        FailTask err ->
            div [ class "task-report" ]
                [ FeatherIcons.thumbsDown |> FeatherIcons.withStrokeWidth 2 |> FeatherIcons.withSize 18 |> FeatherIcons.toHtml []
                , span [] [ err |> toString |> text ]
                ]

        SucceedTask data ->
            div [ class "task-report" ]
                [ span [ class "json-dump" ] [ data |> Encode.encode 4 |> text ]
                ]

        UnknownTask x ->
            x |> toString |> text


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
        TaskEvent id duration isSuccess task ->
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
                [ viewTask task
                  --, span [] [ text (toString duration) ]
                ]

        StateUpdate id _ state diff msg ->
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
                [ FeatherIcons.activity |> FeatherIcons.withSize 18 |> FeatherIcons.withStrokeWidth 2 |> FeatherIcons.toHtml []
                , span [] [ text msg.name ]
                , case diff of
                    ObjectDiff props ->
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


viewTask : TaskReport -> Html Msg
viewTask task =
    case task of
        HttpRequest http ->
            div [ class "task-report http-request" ]
                [ Icons.send
                , span [ class ("method " ++ (http.request.method |> String.toLower)) ] [ http.request.method |> text ]
                , span [ class "url" ] [ http.request.url |> text ]
                , case http.response of
                    Just res ->
                        span [ class "status-code" ] [ res.status.code |> toString |> text ]

                    Nothing ->
                        text ""
                ]

        CurrentTime x ->
            div [ class "task-report" ]
                [ FeatherIcons.clock |> FeatherIcons.withStrokeWidth 2 |> FeatherIcons.withSize 18 |> FeatherIcons.toHtml []
                , span [] [ Strftime.format "%B %d %Y, %H:%M:%S" (Date.fromTime x) |> text ]
                ]

        FailTask err ->
            div [ class "task-report" ]
                [ FeatherIcons.thumbsDown |> FeatherIcons.withStrokeWidth 2 |> FeatherIcons.withSize 18 |> FeatherIcons.toHtml []
                , span [] [ err |> toString |> text ]
                ]

        SucceedTask data ->
            div [ class "task-report" ]
                [ FeatherIcons.thumbsUp |> FeatherIcons.withStrokeWidth 2 |> FeatherIcons.withSize 18 |> FeatherIcons.toHtml []
                  --, span [ class "json-dump" ] [ data |> Encode.encode 4 |> text ]
                ]

        x ->
            x |> toString |> text
