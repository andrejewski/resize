port module Main exposing (Model)

import Browser
import Browser.Dom
import Browser.Events
import Html exposing (..)
import Html.Attributes exposing (class, href, style)
import Json.Decode as Decode
import Json.Encode as Encode
import Random
import Task
import Time exposing (Posix)


type Scene
    = Home
    | Game
    | GameOver


type alias ViewPort =
    { width : Float
    , height : Float
    }


type alias Window =
    { top : Int
    , left : Int
    , width : Float
    , height : Float
    }


type alias Model =
    { scene : Scene
    , mobile : Bool
    , screen : ViewPort
    , viewPort : Window
    , window : Window
    , windowCount : Int
    , currentTime : Posix
    , startTime : Posix
    , roundTime : Posix
    , endTime : Posix
    }


type Msg
    = InitializeViewPort Browser.Dom.Viewport
    | UpdateViewPort Int Int
    | UpdateScreen Encode.Value
    | UpdateTime Posix
    | StartGame Posix
    | StartRound Posix
    | UpdateWindow Window


port loadScreenSize : () -> Cmd msg


port onScreenResize : (Encode.Value -> msg) -> Sub msg


init : Int -> ( Model, Cmd Msg )
init flags =
    let
        model =
            { scene = Home
            , mobile = False
            , viewPort = { top = 0, left = 0, width = 0.0, height = 0.0 }
            , screen = { width = 0.0, height = 0.0 }
            , window = { top = 0, left = 0, width = 0.0, height = 0.0 }
            , windowCount = 0
            , currentTime = Time.millisToPosix 0
            , startTime = Time.millisToPosix 0
            , roundTime = Time.millisToPosix 0
            , endTime = Time.millisToPosix 0
            }
    in
    ( model
    , Cmd.batch
        [ Task.perform InitializeViewPort Browser.Dom.getViewport
        , loadScreenSize ()
        ]
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        defaultSubs =
            Sub.batch
                [ Browser.Events.onResize UpdateViewPort
                , onScreenResize UpdateScreen
                ]
    in
    if model.scene == Game then
        Sub.batch
            [ defaultSubs
            , Time.every 40 UpdateTime
            ]

    else
        defaultSubs


screenResizeEventDecorder : Decode.Decoder Window
screenResizeEventDecorder =
    Decode.map4
        (\t l w h -> { top = t, left = l, width = w, height = h })
        (Decode.field
            "top"
            Decode.int
        )
        (Decode.field
            "left"
            Decode.int
        )
        (Decode.field
            "width"
            Decode.float
        )
        (Decode.field
            "height"
            Decode.float
        )


windowGenerator : Window -> ViewPort -> Random.Generator Window
windowGenerator maxView minView =
    let
        width =
            Random.float minView.width (maxView.width - toFloat maxView.left)

        height =
            Random.float minView.height (maxView.height - toFloat maxView.top)

        left =
            Random.andThen (\w -> Random.int maxView.left (maxView.width - w |> floor)) width

        top =
            Random.andThen (\h -> Random.int maxView.top (maxView.height - h |> floor)) height
    in
    Random.map4 Window top left width height


equalViewPort : Window -> Window -> Bool
equalViewPort a b =
    let
        margin =
            2

        widthMatches =
            (a.width - b.width |> abs) < margin

        heightMatches =
            (a.height - b.height |> abs) < margin
    in
    widthMatches && heightMatches



-- Chrome's is 500 x 296 (excludes 79 of chrome head)


minViewPort : ViewPort
minViewPort =
    { width = 500.0, height = 375.0 }


homeViewPort : ViewPort
homeViewPort =
    { width = minViewPort.width + 100, height = minViewPort.height + 100 }


updateWindow : Model -> Cmd Msg
updateWindow model =
    let
        edgePadding =
            140

        maxWindow =
            { top = edgePadding
            , left = edgePadding
            , width = model.screen.width - edgePadding
            , height = model.screen.height - edgePadding
            }

        paddedWindow =
            windowGenerator maxWindow minViewPort
    in
    Random.generate UpdateWindow paddedWindow


homeWindow : Model -> Window
homeWindow model =
    { top = (model.screen.height / 2) - (homeViewPort.height / 2) |> floor
    , left = (model.screen.width / 2) - (homeViewPort.width / 2) |> floor
    , width = homeViewPort.width
    , height = homeViewPort.height
    }


remainingTime : Model -> Int
remainingTime model =
    let
        maxTime =
            30 * 1000

        duration =
            maxTime
                - (10 * model.windowCount)

        endTime =
            (model.roundTime |> Time.posixToMillis) + duration
    in
    endTime - (model.currentTime |> Time.posixToMillis)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        InitializeViewPort viewPort ->
            let
                newModel =
                    { model
                        | viewPort =
                            { top = 0
                            , left = 0
                            , width = viewPort.viewport.width
                            , height = viewPort.viewport.height
                            }
                    }
            in
            ( newModel
            , updateWindow newModel
            )

        UpdateViewPort width height ->
            let
                viewPort =
                    model.viewPort

                newModel =
                    { model | viewPort = { viewPort | width = toFloat width, height = toFloat height } }
            in
            if equalViewPort model.viewPort model.window then
                case model.scene of
                    Home ->
                        ( model, Task.perform StartGame Time.now )

                    Game ->
                        ( model, Task.perform StartRound Time.now )

                    GameOver ->
                        ( { model | scene = Home, window = homeWindow model }, Cmd.none )

            else
                ( newModel, Cmd.none )

        UpdateScreen event ->
            case Decode.decodeValue screenResizeEventDecorder event of
                Ok window ->
                    let
                        viewPort =
                            model.viewPort

                        newScreen =
                            { width = window.width, height = window.height }

                        newModel =
                            { model
                                | screen = newScreen
                                , viewPort = { viewPort | top = window.top, left = window.left }
                            }
                    in
                    if model.screen == newScreen then
                        ( newModel, Cmd.none )

                    else
                        case model.scene of
                            Home ->
                                ( { newModel | window = homeWindow newModel }, Cmd.none )

                            Game ->
                                ( newModel, updateWindow newModel )

                            GameOver ->
                                ( newModel, updateWindow newModel )

                Err _ ->
                    -- let
                    --     _ =
                    --         Debug.log "parse error" error
                    -- in
                    ( model, Cmd.none )

        StartGame time ->
            ( { model
                | scene = Game
                , startTime = time
                , roundTime = time
                , endTime = Time.millisToPosix 0
                , windowCount = 0
              }
            , updateWindow model
            )

        StartRound time ->
            ( { model
                | roundTime = time
                , windowCount = model.windowCount + 1
              }
            , updateWindow model
            )

        UpdateWindow window ->
            ( { model | window = window }, Cmd.none )

        UpdateTime time ->
            let
                newModel =
                    { model | currentTime = time }
            in
            if newModel.scene == Game && remainingTime newModel < 0 then
                ( { newModel | scene = GameOver, endTime = time }, updateWindow model )

            else
                ( newModel, Cmd.none )


plural : String -> Int -> String
plural unit num =
    if num == 1 then
        String.fromInt num ++ " " ++ unit

    else
        String.fromInt num ++ " " ++ unit ++ "s"


px : Float -> String
px num =
    String.fromFloat num ++ "px"


view : Model -> Html Msg
view model =
    div []
        [ portalView model
        , case model.scene of
            Home ->
                homeView

            Game ->
                div [ class "game" ]
                    [ p [ class "countdown" ]
                        [ let
                            time =
                                remainingTime model

                            seconds =
                                time // 1000

                            milliseconds =
                                modBy 1000 time

                            displaySeconds =
                                seconds |> String.fromInt |> String.padLeft 2 '0'

                            displayMilliseconds =
                                milliseconds |> String.fromInt |> String.padLeft 3 '0'
                          in
                          text (displaySeconds ++ ":" ++ displayMilliseconds)
                        ]
                    ]

            GameOver ->
                div [ class "info overlay" ]
                    [ article []
                        [ h1 [] [ text "The game is now done." ]
                        , p []
                            [ let
                                diffMs =
                                    (model.endTime |> Time.posixToMillis) - (model.startTime |> Time.posixToMillis)

                                seconds =
                                    diffMs // 1000
                              in
                              text ("You perfectly resized " ++ plural "window" model.windowCount ++ " in " ++ plural "second" seconds ++ ".")
                            ]
                        , p [] [ b [] [ text "Resize your browser window to fit the box to return home." ] ]
                        ]
                    ]
        ]


portalView : Model -> Html Msg
portalView model =
    div []
        [ div
            [ class "portal-frame"
            , style "left" ((model.window.left - model.viewPort.left) |> toFloat |> px)
            , style "top" ((model.window.top - model.viewPort.top) |> toFloat |> px)
            , style "width" (model.window.width |> px)
            , style "height" (model.window.height |> px)
            ]
            []
        ]


homeView : Html Msg
homeView =
    div [ class "info overlay" ]
        [ article []
            [ h1 [] [ text "Re:Size" ]
            , p []
                [ text
                    "Most modern websites are responsive: when you resize your browser window, they adjust to make adequate use of the space. This game ain't that. To win, you must continually resize your window perfectly to fit the mold. Grab those corners and race against a clock to become the greatest window manager of all time."
                ]
            , p [] [ b [] [ text "Resize your browser window to fit the box to start the game. Yes, seriously." ] ]
            , p [ class "mobile-only" ] [ text "(⚠️ Won't work on devices that don't let you resize your browser.)" ]
            , hr [] []
            , p [ class "credits" ]
                [ text "Re:Size is written by "
                , a [ href "https://jew.ski/" ] [ text "Chris Andrejewski" ]
                , text ". The source code is "
                , a [ href "https://github.com/andrejewski/resize" ] [ text "open source" ]
                , text "."
                ]
            ]
        ]


main : Program Int Model Msg
main =
    Browser.element
        { init = init, subscriptions = subscriptions, update = update, view = view }
