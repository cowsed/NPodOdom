module Vec2 exposing (..)


type alias Vec2 =
    { x : Float, y : Float }


dot : Vec2 -> Vec2 -> Float
dot a b =
    a.x * b.x + a.y * b.y


scale s v =
    { x = v.x * s, y = v.y * s }


length : Vec2 -> Float
length v =
    sqrt (v.x * v.x + v.y * v.y)


from_degrees : Float -> Vec2
from_degrees ang =
    let
        rad =
            degrees ang

        d =
            Vec2 (cos rad) (sin rad)
    in
    d


norm : Vec2 -> Vec2
norm a =
    a |> scale (1 / length a)


add : Vec2 -> Vec2 -> Vec2
add a b =
    Vec2 (a.x + b.x) (a.y + b.y)


el_mul : Vec2 -> Vec2 -> Vec2
el_mul a b =
    Vec2 (a.x * b.x) (a.y * b.y)


rotate_ccw_90 : Vec2 -> Vec2
rotate_ccw_90 v =
    Vec2 -v.y v.x


ex : Vec2
ex =
    Vec2 1 0


ey : Vec2
ey =
    Vec2 0 1
