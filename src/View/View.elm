module View.View exposing (view)

import Array exposing (..)
import Html exposing (..)
import Html.Attributes exposing (attribute, class, href, id, placeholder, scope, selected, style, type_, value)
import Html.Attributes.Aria exposing (ariaExpanded, ariaHasPopup, ariaHidden, ariaLabel, ariaLabelledby, role)
import Html.Events exposing (onClick, onInput, onSubmit)
import Iso8601
import Model.Model exposing (..)
import Time exposing (utc)


cardBody : List (Html Msg) -> Html Msg
cardBody list =
    div [ class "card", style "width" "18rem" ]
        [ div [ class "card-body" ]
            list
        ]


cardBodyTitle : String -> Html Msg
cardBodyTitle title =
    h5 [ class "card-title text-primary" ] [ text title ]


cardBodyTitleOnly : String -> Html Msg
cardBodyTitleOnly title =
    cardBody
        [ cardBodyTitle title
        ]


viewTaskEntity : Model -> Html Msg
viewTaskEntity model =
    case model of
        Failure ->
            cardBodyTitleOnly "Something went wrong."

        NotImplementedYet ->
            cardBodyTitleOnly "This function has not been implemented yet."

        HomePage ->
            cardBody
                [ cardBodyTitle "Welcome to Task Manager"
                , text "Version 2021-12-11 16:52"
                ]

        Typing taskId ->
            viewInput taskId

        Loading taskId ->
            cardBodyTitleOnly ("Loading Task No.: " ++ String.fromInt taskId)

        Deleting taskId ->
            cardBodyTitleOnly ("Deleting Task No.: " ++ String.fromInt taskId)

        LoadingList ->
            cardBodyTitleOnly "Loading All Tasks. Please be patient the first time."

        CreatingEntity _ ->
            cardBodyTitleOnly "Creating a Task"

        EditingEntity taskEntity ->
            let
                title =
                    if taskEntity.id == 0 then
                        "Creating a new Task"

                    else
                        "Editing Task No. " ++ String.fromInt taskEntity.id
            in
            cardBody
                [ cardBodyTitle title
                , viewForm taskEntity
                ]

        DisplayingEntity taskEntity ->
            cardBody
                [ viewTaskEntityField taskEntity Title
                , viewTaskEntityField taskEntity Description
                , viewTaskEntityField taskEntity Status
                , viewTaskEntityField taskEntity Urgency
                , viewTaskEntityField taskEntity Duration
                , viewTaskEntityField taskEntity AttentionDate
                , viewTaskEntityField taskEntity Deadline
                , viewTaskEntityField taskEntity PlannedDate
                , viewTaskEntityField taskEntity PlannedStartingTime
                ]

        DisplayingEntityList statusFilter textFilter taskEntityList ->
            let
                selectedStatus =
                    case statusFilter of
                        "Unplanned" ->
                            0

                        "Planned" ->
                            1

                        "Done" ->
                            2

                        "In the bin" ->
                            3

                        _ ->
                            -1

                thirdColumnTitle =
                    case statusFilter of
                        "Unplanned" ->
                            "Attention"

                        "Planned" ->
                            "Planned"

                        "Done" ->
                            ""

                        "In the bin" ->
                            ""

                        _ ->
                            "Status"

                filteredList =
                    case statusFilter of
                        "Unplanned" ->
                            List.sortBy .attentionDate (List.filter (\taskEntity -> taskEntity.status == selectedStatus) taskEntityList)

                        "Planned" ->
                            List.sortBy .plannedDate (List.filter (\taskEntity -> taskEntity.status == selectedStatus) taskEntityList)

                        "Done" ->
                            List.sortBy .plannedDate (List.filter (\taskEntity -> taskEntity.status == selectedStatus) taskEntityList)

                        "In the bin" ->
                            List.sortBy .updatedAt (List.filter (\taskEntity -> taskEntity.status == selectedStatus) taskEntityList)

                        _ ->
                            List.sortBy .status taskEntityList

                sortedList =
                    List.filter
                        (\taskEntity -> String.contains (String.toLower textFilter) (String.toLower (taskEntity.title ++ taskEntity.description)))
                        filteredList
            in
            div [ class "card", style "width" "22rem" ]
                [ div []
                    [ div [ class "input-group p-2" ]
                        [ div [ class "input-group-prepend" ]
                            [ span [ class "input-group-text" ] [ text "Filter" ]
                            ]
                        , input
                            [ type_ "text"
                            , placeholder "Search for"
                            , onInput (FilterOnTextTaskEntityList taskEntityList statusFilter)
                            , value textFilter
                            , class "form-control"
                            ]
                            []
                        , select
                            [ onInput (FilterOnStatusTaskEntityList taskEntityList textFilter)
                            , class "form-control"
                            ]
                            (List.map (\i -> viewStatusOption statusFilter i) [ "All", viewStatus 0, viewStatus 1, viewStatus 2, viewStatus 3 ])
                        ]
                    ]
                , table
                    [ class "card-table table table-hover" ]
                    [ thead [ class "card-header" ]
                        [ tr []
                            [ th [ scope "col" ] [ text "Action" ]
                            , th [ scope "col" ] [ text "Title" ]
                            , th [ scope "col" ] [ text thirdColumnTitle ]
                            ]
                        ]
                    , tbody [] (List.map (\taskEntity -> viewTaskEntityLine statusFilter taskEntity) sortedList)
                    ]
                ]


viewTaskEntityLine : String -> TaskEntity -> Html Msg
viewTaskEntityLine statusFilter taskEntity =
    let
        thirdColumnElement =
            case statusFilter of
                "Unplanned" ->
                    toNiceDate taskEntity.attentionDate

                "Planned" ->
                    toNiceDate taskEntity.plannedDate

                "Done" ->
                    ""

                "In the bin" ->
                    ""

                _ ->
                    viewStatus taskEntity.status
    in
    tr []
        [ th [ scope "row" ]
            [ button
                [ type_ "button"
                , class "btn btn-outline-primary btn-sm"
                , onClick
                    (GetTaskEntity taskEntity.id)
                ]
                [ text "View" ]
            ]
        , td [] [ text taskEntity.title ]
        , td [] [ text thirdColumnElement ]
        ]


showTaskEntityField : TaskEntity -> TaskEntityField -> Bool
showTaskEntityField taskEntity taskEntityField =
    case taskEntityField of
        Urgency ->
            taskEntity.status == 0

        AttentionDate ->
            taskEntity.status /= 1 && taskEntity.status /= 2

        Deadline ->
            taskEntity.status /= 1 && taskEntity.status /= 2

        PlannedDate ->
            taskEntity.status /= 0

        PlannedStartingTime ->
            taskEntity.status /= 0

        _ ->
            True


viewTaskEntityField : TaskEntity -> TaskEntityField -> Html Msg
viewTaskEntityField taskEntity taskField =
    let
        displayField =
            showTaskEntityField taskEntity taskField
    in
    if displayField then
        case taskField of
            Title ->
                h5 [ class "card-title text-primary" ]
                    [ text
                        (String.concat
                            [ taskEntity.title
                            , " ("
                            , String.fromInt taskEntity.id
                            , ")"
                            ]
                        )
                    ]

            Description ->
                pre [ class "card-title text-primary" ]
                    [ text (String.concat [ taskEntity.description ]) ]

            Status ->
                div [ class "card-text text-secondary" ]
                    [ text
                        (String.concat
                            [ "Status: "
                            , viewStatus taskEntity.status
                            ]
                        )
                    ]

            Urgency ->
                div [ class "card-text text-secondary" ]
                    [ text
                        (String.concat
                            [ "Urgency: "
                            , viewUrgency taskEntity.urgency
                            ]
                        )
                    ]

            Duration ->
                div [ class "card-text text-secondary" ]
                    [ text
                        (String.concat
                            [ "Duration: "
                            , String.fromInt taskEntity.durationMinutes
                            , " minutes"
                            ]
                        )
                    ]

            AttentionDate ->
                div [ class "card-text text-secondary" ]
                    [ text
                        (String.concat
                            [ "Attention"
                            , iso8601ToWeekday taskEntity.attentionDate
                            , " "
                            , taskEntity.attentionDate
                            ]
                        )
                    ]

            Deadline ->
                div [ class "card-text text-secondary" ]
                    [ text
                        (String.concat
                            [ "Deadline"
                            , iso8601ToWeekday taskEntity.deadline
                            , " "
                            , taskEntity.deadline
                            ]
                        )
                    ]

            PlannedDate ->
                div [ class "card-text text-secondary" ]
                    [ text
                        (String.concat
                            [ if taskEntity.status == 2 then
                                "Finished"

                              else
                                "Planned"
                            , iso8601ToWeekday taskEntity.plannedDate
                            , " "
                            , taskEntity.plannedDate
                            ]
                        )
                    ]

            PlannedStartingTime ->
                div [ class "card-text text-secondary" ]
                    [ text
                        (String.concat
                            [ if taskEntity.status == 2 then
                                "Started at: "

                              else
                                "Starting Time: "
                            , iso8601ToHoursMinutes taskEntity.plannedStartingTime
                            ]
                        )
                    ]

    else
        span [] []


viewStatus : Int -> String
viewStatus status =
    case status of
        0 ->
            "Unplanned"

        1 ->
            "Planned"

        2 ->
            "Done"

        3 ->
            "In the bin"

        _ ->
            "unknown status"


viewStatusButton : Int -> String
viewStatusButton status =
    case status of
        0 ->
            "Unplan Task"

        1 ->
            "Plan Task"

        2 ->
            "Mark Task as Done"

        3 ->
            "Put Task in the bin"

        _ ->
            "unknown status for button"


viewStatusOption : String -> String -> Html Msg
viewStatusOption currentStatusFilter status =
    option
        [ selected (currentStatusFilter == status)
        , value <| status
        ]
        [ text <| status ]



--  enum status: [:unplanned, :planned, :done, :deleted]


viewUrgency : Int -> String
viewUrgency urgency =
    case urgency of
        0 ->
            "Just do it"

        1 ->
            "Plan it"

        2 ->
            "Delegate it"

        3 ->
            "Look at it later"

        _ ->
            "unknown urgency"



--  enum urgency: [:just_do_it, :plan_it, :delegate_it, :dont_do_it]


iso8601ToHoursMinutes : String -> String
iso8601ToHoursMinutes jsonDateTime =
    String.slice 11 16 jsonDateTime


iso8601ToWeekday : String -> String
iso8601ToWeekday jsonDateTime =
    case Iso8601.toTime jsonDateTime of
        Ok posix ->
            ": " ++ toEnglishWeekday (Time.toWeekday Time.utc posix)

        Err _ ->
            ""


toNiceDate : String -> String
toNiceDate date =
    let
        year =
            String.slice 2 4 date

        month =
            String.slice 5 7 date

        day =
            String.slice 8 10 date
    in
    String.concat
        [ day
        , "/"
        , month
        , "/"
        , year
        ]


toEnglishWeekday : Time.Weekday -> String
toEnglishWeekday weekday =
    case weekday of
        Time.Mon ->
            "Monday"

        Time.Tue ->
            "Tuesday"

        Time.Wed ->
            "Wednesday"

        Time.Thu ->
            "Thursday"

        Time.Fri ->
            "Friday"

        Time.Sat ->
            "Saturday"

        Time.Sun ->
            "Sunday"



-- 2012-04-23T18:25:43.511Z
-- 012345678901234567890123
--           1         2


timeToString : Int -> Int -> String
timeToString time timezone =
    let
        milliseconds =
            (time + timezone) * 1000

        posix =
            Time.millisToPosix milliseconds

        hours =
            Time.toHour utc posix

        minutes =
            Time.toMinute utc posix

        seconds =
            Time.toSecond utc posix

        hoursString =
            String.fromInt hours

        minutesString =
            (if minutes < 10 then
                "0"

             else
                ""
            )
                ++ String.fromInt minutes

        secondsString =
            (if seconds < 10 then
                "0"

             else
                ""
            )
                ++ String.fromInt seconds
    in
    String.concat [ hoursString, ":", minutesString, ":", secondsString ]


viewInput : Id -> Html Msg
viewInput taskId =
    div [ class "card", style "width" "18rem" ]
        [ div [ class "card-body" ]
            [ h5 [ class "card-title text-primary" ] [ text "Search Tasks" ]
            , p [ class "card-title" ]
                [ span [ class "text-secondary" ] [ text "Enter a task id and then press " ]
                , span
                    [ class "fa fa-search text-primary"
                    , ariaHidden True
                    , ariaLabel "Search"
                    ]
                    []
                , span [ class "text-primary" ] [ text " Search" ]
                , span [ class "text-secondary" ] [ text " to get the task." ]
                ]
            , div [ class "input-group mb-3" ]
                [ input
                    [ value (String.fromInt taskId)
                    , onInput GetTaskId
                    , type_ "number"
                    , class "form-control"
                    , placeholder "0"
                    ]
                    []
                , div [ class "input-group-append" ]
                    [ button
                        [ class "btn btn-outline-primary"
                        , type_ "button"
                        , ariaLabel "Left Align"
                        , onClick (GetTaskEntity taskId)
                        ]
                        [ span
                            [ class "fa fa-search text-primary"
                            , ariaHidden True
                            , ariaLabel "Search"
                            ]
                            []
                        , text " Search"
                        ]
                    ]
                ]
            ]
        ]


viewUrgencyOption : TaskEntity -> Int -> Html Msg
viewUrgencyOption taskEntity urgency =
    option
        [ selected (taskEntity.urgency == urgency)
        , value <| viewUrgency urgency
        ]
        [ text <| viewUrgency urgency ]


viewFormField : TaskEntity -> TaskEntityField -> String -> String -> String -> Html Msg
viewFormField taskEntity taskEntityField inputType labelText valueText =
    let
        displayField =
            showTaskEntityField taskEntity taskEntityField
    in
    if displayField then
        case taskEntityField of
            Description ->
                div [ class "form-group" ]
                    [ label []
                        [ text labelText ]
                    , textarea
                        [ placeholder labelText
                        , onInput (SetTaskEntity taskEntity taskEntityField)
                        , value valueText
                        , class "form-control"
                        ]
                        []
                    ]

            Urgency ->
                div [ class "form-group" ]
                    [ label []
                        [ text labelText ]
                    , select
                        [ onInput (SetTaskEntity taskEntity taskEntityField)
                        , class "form-control"
                        ]
                        (List.map (\i -> viewUrgencyOption taskEntity i) [ 0, 1, 2, 3 ])
                    ]

            PlannedStartingTime ->
                div [ class "form-group" ]
                    [ label []
                        [ text labelText ]
                    , input
                        [ type_ inputType
                        , placeholder ""
                        , onInput (SetTaskEntity taskEntity PlannedStartingTime)
                        , value taskEntity.plannedStartingTime
                        , class "form-control"
                        ]
                        []
                    ]

            _ ->
                div [ class "form-group" ]
                    [ label []
                        [ text labelText ]
                    , input
                        [ type_ inputType
                        , placeholder labelText
                        , onInput (SetTaskEntity taskEntity taskEntityField)
                        , value valueText
                        , class "form-control"
                        ]
                        []
                    ]

    else
        span [] []


viewForm : TaskEntity -> Html Msg
viewForm taskEntity =
    Html.form
        [ onSubmit
            (if taskEntity.id == 0 then
                CreateTaskEntity taskEntity

             else
                SaveTaskEntity taskEntity
            )
        ]
        [ viewTaskEntityField taskEntity Status
        , viewFormField taskEntity Title "text" "Title" taskEntity.title
        , viewFormField taskEntity Description "text" "Description" taskEntity.description
        , viewFormField taskEntity Urgency "text" "Urgency" taskEntity.description
        , viewFormField taskEntity Duration "text" "Duration in minutes" (String.fromInt taskEntity.durationMinutes)
        , viewFormField taskEntity AttentionDate "date" ("Attention Date" ++ iso8601ToWeekday taskEntity.attentionDate) taskEntity.attentionDate
        , viewFormField taskEntity Deadline "date" ("Deadline" ++ iso8601ToWeekday taskEntity.deadline) taskEntity.deadline
        , viewFormField taskEntity PlannedDate "date" ("Planned Date" ++ iso8601ToWeekday taskEntity.plannedDate) taskEntity.plannedDate
        , viewFormField taskEntity PlannedStartingTime "time" ("Planned Time: " ++ iso8601ToHoursMinutes taskEntity.plannedStartingTime) taskEntity.plannedDate
        , div [ class "form-group" ]
            [ button
                [ type_ "submit"
                , class "btn btn-outline-primary"
                ]
                [ text
                    (if taskEntity.id == 0 then
                        "Create"

                     else
                        "Save"
                    )
                ]
            , span [] [ text " " ]
            , button
                [ class "btn btn-outline-warning"
                , type_ "button"
                , if taskEntity.id == 0 then
                    onClick GetTaskEntityList

                  else
                    onClick (GetTaskEntity taskEntity.id)
                ]
                [ text "Cancel" ]
            ]
        ]


viewTaskEntityMenuButton : Html Msg
viewTaskEntityMenuButton =
    div [ class "btn-group", role "group" ]
        [ button
            [ class "btn btn-outline-primary dropdown-toggle"
            , type_ "button"
            , id "dropdownTaskMenuButton"
            , attribute "data-toggle" "dropdown"
            , ariaHasPopup "menu"
            , ariaExpanded "false"
            ]
            [ text "Task" ]
        , div
            [ class "dropdown-menu"
            , ariaLabelledby "dropdownMenuButton"
            ]
            [ a
                [ class "dropdown-item"
                , href "#"
                , onClick (SetTaskEntity initTaskEntity Title "")
                ]
                [ text "New Task" ]
            , div [ class "dropdown-divider" ] []
            , a
                [ class "dropdown-item"
                , href "#"
                , onClick GetTaskEntityList
                ]
                [ text "Get All Tasks" ]
            ]
        ]


includeMenuItem : TaskEntity -> Int -> Bool
includeMenuItem taskEntity status =
    taskEntity.status /= status


viewMenuItem : TaskEntity -> Int -> Html Msg
viewMenuItem taskEntity status =
    let
        statusText =
            viewStatus status

        buttonText =
            viewStatusButton status
    in
    a
        [ class
            ("dropdown-item"
                ++ (if taskEntity.status == status then
                        " disabled"

                    else
                        ""
                   )
            )
        , href "#"
        , onClick (SetTaskEntity taskEntity Status statusText)
        ]
        [ text buttonText ]


appendPart1andPart2 : TaskEntity -> List (Html Msg)
appendPart1andPart2 taskEntity =
    List.append (viewTaskFunctionsMenuPart1 taskEntity) (viewTaskFunctionsMenuPart2 taskEntity)


viewTaskFunctionsMenuPart1 : TaskEntity -> List (Html Msg)
viewTaskFunctionsMenuPart1 taskEntity =
    List.map (\i -> viewMenuItem taskEntity i) (List.filter (\i -> includeMenuItem taskEntity i) [ 0, 1, 2, 3 ])


viewTaskFunctionsMenuPart2 : TaskEntity -> List (Html Msg)
viewTaskFunctionsMenuPart2 taskEntity =
    let
        deleteTask : List (Html Msg)
        deleteTask =
            if taskEntity.status == 3 then
                [ a
                    [ class "dropdown-item text-danger"
                    , href "#"
                    , onClick (DeleteTaskEntity taskEntity.id)
                    ]
                    [ text
                        ("Delete Task"
                            ++ (if taskEntity.id > 0 then
                                    " No. " ++ String.fromInt taskEntity.id

                                else
                                    ""
                               )
                        )
                    ]
                ]

            else
                []

        editTask : List (Html Msg)
        editTask =
            [ a
                [ class "dropdown-item"
                , href "#"
                , onClick (SetTaskEntity taskEntity Title taskEntity.title)
                ]
                [ text "Edit Task" ]
            ]
    in
    div [ class "dropdown-divider" ] []
        :: editTask
        ++ deleteTask


viewTaskFunctionsMenuButton : Model -> Html Msg
viewTaskFunctionsMenuButton model =
    let
        taskEntity =
            case model of
                DisplayingEntity taskEntity2 ->
                    taskEntity2

                _ ->
                    initTaskEntity
    in
    div [ class "btn-group", role "group" ]
        [ button
            [ class
                ("btn btn-outline-primary dropdown-toggle"
                    ++ (if taskEntity.id == 0 then
                            " disabled"

                        else
                            ""
                       )
                )
            , type_ "button"
            , id "dropdownFunctionMenuButton"
            , attribute "data-toggle" "dropdown"
            , ariaHasPopup "menu"
            , ariaExpanded "false"
            ]
            [ text "Functions" ]
        , div
            [ class "dropdown-menu"
            , ariaLabelledby "dropdownMenuButton"
            ]
            (appendPart1andPart2 taskEntity)
        ]


viewMenu : Model -> Html Msg
viewMenu model =
    div [ class "p-2" ]
        [ div [ class "btn-toolbar", role "toolbar", ariaLabel "Toolbar" ]
            [ div [ class "btn-group mr-2", role "group", ariaLabel "TaskEntity Menu Buttons" ]
                [ viewTaskEntityMenuButton
                , viewTaskFunctionsMenuButton model
                ]
            , div [ class "btn-group mr-2", role "group", ariaLabel "TaskEntity Menu Buttons" ]
                [ button
                    [ type_ "button"
                    , class "btn btn-outline-primary"
                    , href "#"
                    , onClick GoToHomePage
                    ]
                    [ text "Home" ]
                ]
            ]
        ]


solidBlackBorder : Html.Attribute Msg
solidBlackBorder =
    style "border" "1px solid black"


view : Model -> Html Msg
view model =
    div [ class "container-fluid" ]
        [ div [ class "row" ]
            [ div [ class "col" ] [ viewMenu model ] ]
        , div [ class "row" ]
            [ div [ class "col" ] [ viewTaskEntity model ] ]
        ]
