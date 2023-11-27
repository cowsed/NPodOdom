module Main exposing (..)

import Browser
import Browser.Events
import Color
import Draggable
import Draggable.Events
import Html
import Html.Attributes exposing (style)
import Html.Events
import List.Extra
import Math
import Pod exposing (PodConfig)
import Round
import TypedSvg exposing (circle, svg)
import TypedSvg.Attributes exposing (cx, cy, fill, height, r, stroke, strokeDasharray, strokeWidth, viewBox, width, x, x1, x2, y, y1, y2)
import TypedSvg.Core exposing (Svg)
import TypedSvg.Events
import TypedSvg.Types exposing (Paint(..), px)
import Vec2 exposing (Vec2)


type alias Model =
    { pods : List PodConfig
    , selected : Int
    , winsize : Vec2
    , drag : Draggable.State DragId
    , guides_shown : ShownGuides
    , forward_command : { cx : Float, cy : Float, crot : Float }
    }


type ShownGuides
    = All
    | Selected
    | None


type alias DragId =
    { pod_index : Int }


type Msg
    = PodConfigChanged Int PodConfig
    | ShowGuides ShownGuides
    | NewPod
    | WindowResize Int Int
    | OnDragBy Vec2
    | DragMsg (Draggable.Msg DragId)
    | PodSelected DragId
    | NoChange
    | ForwardCmd { cx : Float, cy : Float, crot : Float }


draw_origin : List (Svg msg)
draw_origin =
    let
        zero =
            inchesToPx 0

        rad =
            inchesToPx 1

        nrad =
            inchesToPx -1
    in
    [ circle
        [ cx (px 0)
        , cy (px 0)
        , r rad
        , TypedSvg.Attributes.noFill
        , strokeWidth (px 1)
        , stroke <| Paint <| Color.rgba 0.0 0 0 1
        ]
        []
    , TypedSvg.line [ x1 zero, x2 zero, y1 nrad, y2 rad, stroke (Paint Color.black) ] []
    , TypedSvg.line [ x1 nrad, x2 rad, y1 zero, y2 zero, stroke (Paint Color.black) ] []
    ]


inchesToPx : Float -> TypedSvg.Types.Length
inchesToPx inch =
    px <| inchesToPxNoUnit inch


inchesToPxNoUnit : Float -> Float
inchesToPxNoUnit inch =
    inch * scale_amt


scale_amt : number
scale_amt =
    15


pallete =
    { selected_wheel_color = Color.rgb 0 0 0
    , unselected_wheel_color = Color.rgb 0.6 0.6 0.6
    , unselected_row_color = Color.rgb 1 1 1
    , selected_row_color = Color.rgb 0.8 0.8 0.8
    , xcolor = Color.green
    , ycolor = Color.red
    , rotcolor = Color.blue
    }


draw_pod : Int -> Bool -> PodConfig -> Svg Msg
draw_pod index show_guides pc =
    let
        wwidth =
            1

        color =
            pallete.selected_wheel_color

        robot_vectors =
            let
                { x, y, rot } =
                    Pod.calculate_factors pc

                rot_fac_vec =
                    Vec2.rotate_ccw_90 pc.position |> Vec2.scale (1/rot) |> Vec2.scale (1 / (Vec2.length pc.position))
            in
            if not show_guides then
                []

            else
                [ TypedSvg.line [ x1 (inchesToPx 0), y1 (inchesToPx 0), x2 (inchesToPx pc.position.x), y2 (inchesToPx -pc.position.y), stroke (Paint Color.purple) ] []
                , TypedSvg.line [ x1 (inchesToPx pc.position.x), y1 (inchesToPx -pc.position.y), x2 (inchesToPx (pc.position.x + rot_fac_vec.x)), y2 (inchesToPx -(pc.position.y + rot_fac_vec.y)), stroke (Paint pallete.rotcolor), strokeWidth (inchesToPx 0.1) ] []
                , TypedSvg.line [ x1 (inchesToPx pc.position.x), y1 (inchesToPx -pc.position.y), x2 (inchesToPx (pc.position.x + (1/x * 2))), y2 (inchesToPx -pc.position.y), stroke (Paint pallete.xcolor), strokeWidth (inchesToPx 0.1) ] []
                , TypedSvg.line [ x1 (inchesToPx pc.position.x), y1 (inchesToPx -pc.position.y), x2 (inchesToPx pc.position.x), y2 (inchesToPx -(pc.position.y + (2 * 1/y))), stroke (Paint pallete.ycolor), strokeWidth (inchesToPx 0.1) ] []
                , TypedSvg.circle [ cx (inchesToPx 0), cy (inchesToPx 0), r (inchesToPx (Vec2.length pc.position)), TypedSvg.Attributes.noFill, stroke (Paint pallete.rotcolor), strokeDasharray "3 3" ] []
                ]

        pod_vectors =
            TypedSvg.g []
                [ TypedSvg.line [ x1 (inchesToPx 0), y1 (inchesToPx 0), x1 (inchesToPx pc.wheel_radius), y1 (inchesToPx 0), stroke (Paint Color.orange) ] []
                ]
    in
    TypedSvg.g []
        ([ TypedSvg.g
            [ TypedSvg.Attributes.transform [ TypedSvg.Types.Translate (inchesToPxNoUnit pc.position.x) -(inchesToPxNoUnit pc.position.y), TypedSvg.Types.Rotate -pc.angle 0 0 ]
            ]
            [ TypedSvg.rect
                [ x (inchesToPx -pc.wheel_radius)
                , y (inchesToPx -(wwidth / 2))
                , height (inchesToPx wwidth)
                , width (inchesToPx (pc.wheel_radius * 2))
                , fill <| Paint (Color.rgba 1 1 1 0.1)
                , strokeWidth (px 1)
                , stroke <| Paint <| color
                , TypedSvg.Events.onMouseDown (PodSelected { pod_index = index })
                , TypedSvg.Attributes.cursor TypedSvg.Types.CursorMove
                , Draggable.mouseTrigger (DragId index) DragMsg
                ]
                []
            , pod_vectors
            ]
         ]
            |> List.append robot_vectors
        )


number_input : { min : Float, max : Float, step : Float, onChange : Float -> msg, round : Int } -> Float -> Html.Html msg
number_input { min, max, step, onChange, round } current =
    Html.input
        [ Html.Attributes.type_ "number"
        , Html.Attributes.min (String.fromFloat min)
        , Html.Attributes.max (String.fromFloat max)
        , Html.Attributes.step (String.fromFloat step)
        , Html.Attributes.value <| Round.round round current
        , Html.Events.onInput (\str -> str |> String.toFloat |> Maybe.withDefault 0.0 |> onChange)
        ]
        []


draw_pod_settings : Int -> PodConfig -> Html.Html Msg
draw_pod_settings index pc =
    let
        pos =
            pc.position

        { x, y, rot } =
            Pod.calculate_factors pc

        on_wheel_radius_change newrad =
            Pod.set_radius pc newrad |> PodConfigChanged index

        on_x_change newx =
            PodConfigChanged index { pc | position = { pos | x = newx } }

        on_y_change newy =
            PodConfigChanged index { pc | position = { pos | y = newy } }

        on_angle_change ang =
            Pod.set_angle pc ang |> PodConfigChanged index
    in
    Html.div []
        [ Html.span []
            [ number_input { min = 0, max = 20, step = 0.1, onChange = on_wheel_radius_change, round = 4 } pc.wheel_radius
            , Html.text " : Odom Wheel Radius (in)"
            ]
        , Html.br [] []
        , Html.span []
            [ number_input { min = -20, max = 20, step = 0.1, onChange = on_x_change, round = 4 } pc.position.x
            , Html.text " : Wheel X (in)"
            ]
        , Html.br [] []
        , Html.span []
            [ number_input { min = -20, max = 20, step = 0.1, onChange = on_y_change, round = 4 } pc.position.y
            , Html.text " : Wheel Y (in)"
            ]
        , Html.br [] []
        , Html.span []
            [ number_input { min = -360, max = 360, step = 1, onChange = on_angle_change, round = 4 } pc.angle
            , Html.text " : Angle from +X"
            ]
        , Html.br [] []
        , Html.text ("X Factor = " ++ Round.round 6 x)
        , Html.br [] []
        , Html.text ("Y Factor = " ++ Round.round 6 y)
        , Html.br [] []
        , Html.text ("Rot Factor = " ++ Round.round 6 rot)
        ]


pod_table : Int -> List PodConfig -> Html.Html Msg
pod_table selected pods =
    let
        header =
            [ Html.thead []
                [ Html.th [] [ Html.text "Id" ]
                , Html.th [] [ Html.text "Radius" ]
                , Html.th [] [ Html.text "x" ]
                , Html.th [] [ Html.text "y" ]
                , Html.th [] [ Html.text "θ" ]
                , Html.th [] [ Html.text "fx" ]
                , Html.th [] [ Html.text "fy" ]
                , Html.th [] [ Html.text "fr" ]
                ]
            ]

        make_row : Int -> PodConfig -> Html.Html Msg
        make_row index pc =
            let
                color =
                    if index == selected then
                        pallete.selected_row_color

                    else
                        pallete.unselected_row_color

                { x, y, rot } =
                    Pod.calculate_factors pc
            in
            Html.tr [ Html.Events.onClick (PodSelected { pod_index = index }), style "background-color" (Color.toCssString color) ]
                [ Html.td []
                    [ Html.text (String.fromInt index)
                    ]
                , Html.td [] [ Html.text (Round.round 4 pc.wheel_radius) ]
                , Html.td [] [ Html.text (Round.round 4 pc.position.x) ]
                , Html.td [] [ Html.text (Round.round 4 pc.position.y) ]
                , Html.td [] [ Html.text (Round.round 4 pc.angle) ]
                , Html.td [] [ Html.text (Round.round 4 x) ]
                , Html.td [] [ Html.text (Round.round 4 y) ]
                , Html.td [] [ Html.text (Round.round 4 rot) ]
                ]

        newbutton =
            Html.tr [ style "justify-content" "center", style "width" "100%", style "display" "flex" ] [ Html.button [ Html.Events.onClick NewPod ] [ Html.text "new pod" ] ]
    in
    Html.table [ style "border" "1px solid", style "border-radius" "5px" ]
        (List.concat [ header, pods |> List.indexedMap make_row, [ newbutton ] ])


show_this_guide : Int -> ShownGuides -> Int -> Bool
show_this_guide selected sg index =
    case sg of
        All ->
            True

        Selected ->
            index == selected

        None ->
            False


view : Model -> Html.Html Msg
view mod =
    let
        pods =
            mod.pods
                |> List.indexedMap (\i -> draw_pod i (show_this_guide mod.selected mod.guides_shown i))

        mindim =
            min (mod.winsize.x / 2 - 2) (mod.winsize.y - 2)

        margin =
            2

        num_pods =
            List.length mod.pods

        right_pane =
            Html.div [ style "right" "0", style "height" "100%", style "width" "50%", style "position" "fixed" ]
                [ svg
                    [ viewBox -250 -250 500 500, Html.Attributes.width (round mindim), Html.Attributes.height (round mindim), style "border" "2px solid", style "margin" (String.fromInt margin ++ "px"), style "touch-action" "none" ]
                    (List.concat
                        [ pods
                        , draw_origin
                        ]
                    )
                ]

        guide_buttons =
            Html.form []
                [ Html.text "Draw Guides: "
                , Html.span []
                    [ radio "All" (ShowGuides All)
                    , radio "Selected" (ShowGuides Selected)
                    , radio "None" (ShowGuides None)
                    ]
                ]

        cmd =
            mod.forward_command

        matrixed =
            Pod.calculate_fwd mod.pods cmd

        forward =
            case get2 mod.pods of
                Just ( a, b ) ->
                    let
                        out =
                            Pod.do_tank_fwd { left_cfg = a, right_cfg = b } { cx = cmd.cx, cang = cmd.crot }
                    in
                    Html.div []
                        [ Html.text "x"
                        , number_input { min = -20, max = 20, step = 0.1, round = 4, onChange = \nx -> ForwardCmd { cmd | cx = nx } } cmd.cx
                        , Html.text "in/s"
                        , Html.br [] []
                        , Html.text "y"
                        , number_input { min = -20, max = 20, step = 0.1, round = 4, onChange = \nx -> ForwardCmd { cmd | cy = nx } } cmd.cy
                        , Html.text "in/s"
                        , Html.br [] []
                        , Html.text "rot"
                        , number_input { min = -360, max = 360, step = 1, round = 4, onChange = \ndeg -> ForwardCmd { cmd | crot = degrees ndeg } } (cmd.crot * 180 / pi)
                        , Html.text "dps"
                        , Html.br [] []
                        , Html.text "E1: "
                        , Html.text (String.fromFloat out.left_enc ++ " radians/s")
                        , Html.br [] []
                        , Html.text "E2: "
                        , Html.text (String.fromFloat out.right_enc ++ " radians/s")
                        , Html.br [] []
                        , Html.text (Debug.toString matrixed)
                        , Html.br [] []
                        , Html.br [] []
                        , Math.render <| Math.Sqrt (Math.Symbol "A")
                        ]

                Nothing ->
                    Html.text "wrong setup for forward calcs"

        left_pane =
            Html.div [ style "left" "0", style "height" "100%", style "width" "50%", style "position" "fixed" ]
                [ Html.h2 [] [ Html.text (String.fromInt num_pods ++ " pod odometry" ++ String.repeat (max 0 (num_pods - 2)) "!") ]
                , case List.Extra.getAt mod.selected mod.pods of
                    Just pc ->
                        draw_pod_settings mod.selected pc

                    Nothing ->
                        Html.text ""
                , pod_table mod.selected mod.pods
                , guide_buttons
                , forward
                ]
    
    in
    Html.div []
        [ Math.render (Math.Sqrt (Math.Symbol "A")) --left_pane
        -- , right_pane
        ]


get2 : List a -> Maybe ( a, a )
get2 l =
    case l of
        a :: l1 ->
            case List.head l1 of
                Just b ->
                    ( a, b ) |> Just

                _ ->
                    Nothing

        _ ->
            Nothing


radio : String -> msg -> Html.Html msg
radio value onInput =
    Html.label
        [ style "padding" "20px"
        ]
        [ Html.input [ Html.Attributes.type_ "radio", Html.Attributes.name "font-size", Html.Events.onInput (\_ -> onInput) ] []
        , Html.text value
        ]


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , subscriptions = \mod -> Sub.batch [ Browser.Events.onResize WindowResize, Draggable.subscriptions DragMsg mod.drag ]
        , view = \mod -> { title = "N Pod Odometry", body = [ view mod ] }
        , update = update
        }


update : Msg -> Model -> ( Model, Cmd.Cmd Msg )
update msg mod =
    let
        no_change =
            ( mod, Cmd.none )
    in
    case msg of
        WindowResize x y ->
            ( { mod | winsize = Vec2 (toFloat x) (toFloat y) }, Cmd.none )

        PodConfigChanged index newpod ->
            List.Extra.setAt index newpod mod.pods |> (\newpods -> ( { mod | pods = newpods }, Cmd.none ))

        NewPod ->
            { mod | pods = List.append mod.pods [ PodConfig 1 90.0 (Vec2 4 4) ] } |> no_cmd

        NoChange ->
            no_change

        DragMsg dm ->
            Draggable.update dragConfig dm mod

        PodSelected { pod_index } ->
            { mod | selected = pod_index } |> no_cmd

        OnDragBy delta ->
            { mod
                | pods =
                    List.Extra.updateAt
                        mod.selected
                        (\pc -> Pod.set_position pc (Vec2.add pc.position delta))
                        mod.pods
            }
                |> no_cmd

        ShowGuides sg ->
            { mod | guides_shown = sg } |> no_cmd

        ForwardCmd newcmd ->
            { mod | forward_command = newcmd } |> no_cmd


dragConfig : Draggable.Config DragId Msg
dragConfig =
    Draggable.customConfig
        [ Draggable.Events.onDragBy (\( dx, dy ) -> Vec2 dx dy |> Vec2.el_mul (Vec2 1 -1) |> Vec2.scale (1 / 20) |> (\{ x, y } -> Vec2 x y) |> OnDragBy)
        , Draggable.Events.onDragStart PodSelected
        , Draggable.Events.onClick PodSelected
        ]


no_cmd : a -> ( a, Cmd msg )
no_cmd mod =
    ( mod, Cmd.none )


init : () -> ( Model, Cmd Msg )
init _ =
    ( initial_model, Cmd.none )


initial_model : Model
initial_model =
    { pods = [ PodConfig 1 0.0 (Vec2 0 4), PodConfig 1 0.0 (Vec2 0 -4.0) ]
    , selected = 0
    , winsize = Vec2 1920 800
    , drag = Draggable.init
    , guides_shown = Selected
    , forward_command = { cx = 0, cy = 0, crot = 0 }
    }
