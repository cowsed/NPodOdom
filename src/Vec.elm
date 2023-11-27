module Vec exposing (..)

import List.Extra


type Vec n
    = Vec (List Float)


add : Vec n -> Vec n -> Vec n
add (Vec v1) (Vec v2) =
    Vec (List.Extra.zip v1 v2 |> List.map (\( a, b ) -> a + b))


smul : Float -> Vec n -> Vec n
smul s (Vec v1) =
    Vec (List.map ((*) s) v1)


sub : Vec n -> Vec n -> Vec n
sub a b =
    add a (smul -1.0 b)


dot : Vec n -> Vec n -> Float
dot (Vec v1) (Vec v2) =
    List.Extra.zip v1 v2 |> List.map (\( a, b ) -> inf_aware_mul a b) |> List.sum


from_list : List Float -> Vec n
from_list fs =
    Vec fs


coefs : Vec n -> List Float
coefs (Vec cs) =
    cs


type One
    = One


type Two
    = Two


type Three
    = Three


inf_aware_mul : Float -> Float -> Float
inf_aware_mul a b =
    if isInfinite a then
        if b == 0.0 then
            0.0

        else
            a * b

    else if a == 0.0 then
        if isInfinite b then
            0.0

        else
            a * b

    else
        a * b
