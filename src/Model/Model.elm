module Model.Model exposing (Id, Model(..), Msg(..), TaskEntity, TaskEntityField(..), TaskEntityList, init, initTaskEntity)

import Http


type alias Id =
    Int


type Msg
    = NoOp
    | GoToHomePage
    | SetTaskEntity TaskEntity TaskEntityField String
    | GetTaskEntity Id
    | GetTaskId String
    | GetTaskEntityList
    | AfterGetTaskEntity (Result Http.Error TaskEntity)
    | AfterGetTaskEntityList (Result Http.Error TaskEntityList)
    | FilterOnStatusTaskEntityList TaskEntityList String String
    | FilterOnTextTaskEntityList TaskEntityList String String
    | CreateTaskEntity TaskEntity
    | AfterCreateTaskEntity (Result Http.Error TaskEntity)
    | SaveTaskEntity TaskEntity
    | AfterSaveTaskEntity (Result Http.Error TaskEntity)
    | DeleteTaskEntity Id
    | AfterDeleteTaskEntry (Result Http.Error ())


type Model
    = Failure
    | NotImplementedYet
    | HomePage
    | Typing Id
    | Loading Id
    | LoadingList
    | Deleting Id
    | CreatingEntity TaskEntity
    | EditingEntity TaskEntity
    | DisplayingEntity TaskEntity
    | DisplayingEntityList String String TaskEntityList


type alias TaskEntityList =
    List TaskEntity


type alias TaskEntity =
    { id : Id
    , title : String
    , description : String
    , urgency : Int
    , durationMinutes : Int
    , attentionDate : String
    , deadline : String
    , plannedDate : String
    , plannedStartingTime : String
    , status : Int
    , createdAt : String
    , updatedAt : String
    }


type TaskEntityField
    = Title
    | Description
    | Status
    | Urgency
    | Duration
    | AttentionDate
    | Deadline
    | PlannedDate
    | PlannedStartingTime


initTaskEntity : TaskEntity
initTaskEntity =
    TaskEntity 0 "Type your Title here" "" 0 0 "" "" "" "" 0 "" ""


init : () -> ( Model, Cmd Msg )
init _ =
    ( HomePage, Cmd.none )
