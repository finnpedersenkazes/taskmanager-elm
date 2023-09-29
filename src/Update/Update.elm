module Update.Update exposing (api, deleteTaskEntity, getTaskEntity, getTaskEntityList, patchTaskEntity, postTaskEntity, taskEntityDecoder, taskEntityJson, taskEntityListDecoder, update)

import Http
import Json.Decode as Decode exposing (Decoder, int, string)
import Json.Decode.Pipeline exposing (optional, required)
import Json.Encode as Encode exposing (Value, object)
import Model.Model exposing (..)


api : String
api =
    "https://taskmanager02-api-c6207da5113d.herokuapp.com/tasks"


deleteTaskEntity : Id -> Cmd Msg
deleteTaskEntity taskId =
    let
        urlTaskEntity =
            api ++ "/" ++ String.fromInt taskId
    in
    Http.request
        { method = "DELETE"
        , headers = []
        , url = urlTaskEntity
        , body = Http.emptyBody
        , expect = Http.expectWhatever AfterDeleteTaskEntry
        , timeout = Nothing
        , tracker = Nothing
        }


postTaskEntity : TaskEntity -> Cmd Msg
postTaskEntity task =
    let
        taskBody =
            Http.jsonBody (taskEntityJson task)
    in
    Http.request
        { method = "POST"
        , headers = []
        , url = api
        , body = taskBody
        , expect = Http.expectJson AfterGetTaskEntity taskEntityDecoder
        , timeout = Nothing
        , tracker = Nothing
        }


patchTaskEntity : TaskEntity -> Cmd Msg
patchTaskEntity task =
    let
        urlTaskEntity =
            api ++ "/" ++ String.fromInt task.id

        taskBody =
            Http.jsonBody (taskEntityJson task)
    in
    Http.request
        { method = "PATCH"
        , headers = []
        , url = urlTaskEntity
        , body = taskBody
        , expect = Http.expectJson AfterGetTaskEntity taskEntityDecoder
        , timeout = Nothing
        , tracker = Nothing
        }


getTaskEntity : Id -> Cmd Msg
getTaskEntity taskId =
    let
        urlTaskEntity =
            api ++ "/" ++ String.fromInt taskId
    in
    Http.get
        { url = urlTaskEntity
        , expect = Http.expectJson AfterGetTaskEntity taskEntityDecoder
        }


getTaskEntityList : Cmd Msg
getTaskEntityList =
    let
        urlTaskEntity =
            api
    in
    Http.get
        { url = urlTaskEntity
        , expect = Http.expectJson AfterGetTaskEntityList taskEntityListDecoder
        }


taskEntityListDecoder : Decoder TaskEntityList
taskEntityListDecoder =
    Decode.list taskEntityDecoder


taskEntityDecoder : Decoder TaskEntity
taskEntityDecoder =
    Decode.succeed TaskEntity
        |> required "id" int
        |> required "title" string
        |> optional "description" string ""
        |> optional "urgency" int 0
        |> optional "duration_minutes" int 0
        |> optional "attention_date" string ""
        |> optional "deadline" string ""
        |> optional "planned_date" string ""
        |> optional "planned_starting_time" string ""
        |> optional "status" int 0
        |> required "created_at" string
        |> required "updated_at" string


taskEntityJson : TaskEntity -> Value
taskEntityJson task =
    object
        [ ( "task"
          , object
                [ ( "title", Encode.string task.title )
                , ( "description", Encode.string task.description )
                , ( "urgency", Encode.int task.urgency )
                , ( "duration_minutes", Encode.int task.durationMinutes )
                , ( "attention_date", Encode.string task.attentionDate )
                , ( "deadline", Encode.string task.deadline )
                , ( "planned_date", Encode.string task.plannedDate )
                , ( "planned_starting_time", Encode.string task.plannedStartingTime )
                , ( "status", Encode.int task.status )
                ]
          )
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        GoToHomePage ->
            ( HomePage, Cmd.none )

        SetTaskEntity taskEntity taskEntityField value ->
            let
                updatedTaskEntity =
                    case taskEntityField of
                        Title ->
                            { taskEntity | title = value }

                        Description ->
                            { taskEntity | description = value }

                        Status ->
                            case value of
                                "Unplanned" ->
                                    { taskEntity | status = 0 }

                                "Planned" ->
                                    { taskEntity | status = 1 }

                                "Done" ->
                                    { taskEntity | status = 2 }

                                "In the bin" ->
                                    { taskEntity | status = 3 }

                                _ ->
                                    taskEntity

                        Urgency ->
                            case value of
                                "Just do it" ->
                                    { taskEntity | urgency = 0 }

                                "Plan it" ->
                                    { taskEntity | urgency = 1 }

                                "Delegate it" ->
                                    { taskEntity | urgency = 2 }

                                "Look at it later" ->
                                    { taskEntity | urgency = 3 }

                                _ ->
                                    taskEntity

                        Duration ->
                            { taskEntity | durationMinutes = Maybe.withDefault 0 (String.toInt value) }

                        AttentionDate ->
                            { taskEntity | attentionDate = value }

                        Deadline ->
                            { taskEntity | deadline = value }

                        PlannedDate ->
                            { taskEntity | plannedDate = value }

                        PlannedStartingTime ->
                            { taskEntity | plannedStartingTime = value }
            in
            ( EditingEntity updatedTaskEntity, Cmd.none )

        GetTaskId idString ->
            let
                id =
                    Maybe.withDefault 0 (String.toInt idString)
            in
            ( Typing id, Cmd.none )

        GetTaskEntity id ->
            if id == 0 then
                ( Typing 0, Cmd.none )

            else
                ( Loading id, getTaskEntity id )

        DeleteTaskEntity id ->
            if id == 0 then
                ( Failure, Cmd.none )

            else
                ( Deleting id, deleteTaskEntity id )

        GetTaskEntityList ->
            ( LoadingList, getTaskEntityList )

        AfterGetTaskEntity (Ok taskEntity) ->
            ( DisplayingEntity taskEntity, Cmd.none )

        AfterGetTaskEntity (Err _) ->
            ( Failure, Cmd.none )

        AfterGetTaskEntityList (Ok taskEntityList) ->
            ( DisplayingEntityList "All" "" taskEntityList, Cmd.none )

        AfterGetTaskEntityList (Err _) ->
            ( Failure, Cmd.none )

        FilterOnStatusTaskEntityList taskEntityList filterText filterStatus ->
            ( DisplayingEntityList filterStatus filterText taskEntityList, Cmd.none )

        FilterOnTextTaskEntityList taskEntityList filterStatus filterText ->
            ( DisplayingEntityList filterStatus filterText taskEntityList, Cmd.none )

        CreateTaskEntity taskEntity ->
            ( CreatingEntity taskEntity, postTaskEntity taskEntity )

        AfterCreateTaskEntity (Ok taskEntity) ->
            ( DisplayingEntity taskEntity, Cmd.none )

        AfterCreateTaskEntity (Err _) ->
            ( Failure, Cmd.none )

        SaveTaskEntity taskEntity ->
            ( EditingEntity taskEntity, patchTaskEntity taskEntity )

        AfterSaveTaskEntity (Ok taskEntity) ->
            ( DisplayingEntity taskEntity, Cmd.none )

        AfterSaveTaskEntity (Err _) ->
            ( Failure, Cmd.none )

        AfterDeleteTaskEntry (Ok _) ->
            ( LoadingList, getTaskEntityList )

        AfterDeleteTaskEntry (Err _) ->
            ( Failure, Cmd.none )
