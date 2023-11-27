module Pod exposing (..)

import Html exposing (b)
import Mat exposing (Mat)
import TypedSvg.Types exposing (Length(..))
import Vec exposing (One, Three, Vec)
import Vec2 exposing (Vec2)


type alias PodConfig =
    { wheel_radius : Float
    , angle : Float
    , position : Vec2
    }


set_position : PodConfig -> Vec2 -> PodConfig
set_position pc pos =
    { pc | position = pos }


set_angle : PodConfig -> Float -> PodConfig
set_angle pc ang =
    { pc | angle = ang }


set_radius : PodConfig -> Float -> PodConfig
set_radius pc rad =
    { pc | wheel_radius = rad }


type NumEncoders
    = NumEncoders


calculate_fwd : List PodConfig -> { cx : Float, cy : Float, crot : Float } -> Vec NumEncoders
calculate_fwd configs command =
    let
        tmat : Mat NumEncoders Three
        tmat =
            configs
                |> List.map calculate_factors
                |> List.map (\{ x, y, rot } -> Vec.from_list [ x, y, rot ])
                |> Mat.from_rows

        cmd_vec : Mat Three One
        cmd_vec =
            Mat.from_cols [ Vec.from_list [ command.cx, command.cy, command.crot ] ]

        encs : Mat NumEncoders One
        encs =
            Mat.mul tmat cmd_vec
    in
    case encs |> Mat.cols |> List.head of
        Just v ->
            v

        Nothing ->
            Debug.todo "wrong size matrix for calculate_fwd"


calculate_factors : PodConfig -> { x : Float, y : Float, rot : Float }
calculate_factors cfg =
    let
        wheel_direction =
            Vec2.from_degrees cfg.angle
    in
    { x = 1.0 / (cfg.wheel_radius * Vec2.dot Vec2.ex wheel_direction) -- E / x 
    , y = 1.0 / (cfg.wheel_radius * Vec2.dot Vec2.ey wheel_direction) -- E / y 
    , rot =
        let
            t =
                Vec2.norm (Vec2.rotate_ccw_90 cfg.position)
        in
        --  E / rot 
        1.0 / (cfg.wheel_radius * Vec2.dot t wheel_direction / Vec2.length cfg.position)
    }


do_tank_fwd : { left_cfg : PodConfig, right_cfg : PodConfig } -> { cx : Float, cang : Float } -> { left_enc : Float, right_enc : Float }
do_tank_fwd { left_cfg, right_cfg } { cx, cang } =
    let
        lf =
            calculate_factors left_cfg

        rf =
            calculate_factors right_cfg
    in
    { left_enc = Vec.inf_aware_mul (lf.x) cx + Vec.inf_aware_mul (lf.rot)  cang
    , right_enc = Vec.inf_aware_mul (rf.x) cx + Vec.inf_aware_mul (rf.rot)  cang
    }


