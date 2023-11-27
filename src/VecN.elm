module VecN exposing (..)

import List.Extra


type VecN
    = VecN (List Float)


from_list: List Float -> VecN
from_list cs = VecN cs

zip : List a -> List b -> List ( a, b )
zip la lb =
    List.Extra.zip la lb

add: VecN -> VecN -> VecN
add (VecN afs) (VecN bfs) = 
    List.Extra.zip  afs bfs |> List.map (\(a,b) -> a + b) |> VecN
 
dot : VecN -> VecN -> Float
dot (VecN a) (VecN b) =
    let
        assert =
            assert_same_len a b
    in
    zip a b
        |> List.map (\( ai, bi ) -> ai * bi)
        |> List.sum


assert_same_len a b =
    if List.length a /= List.length b then
        Debug.todo "doing an operation that requires two vectors of the same size but got 2 different ones"

    else
        True


coefs : VecN -> List Float
coefs (VecN cs) =
    cs


size : VecN -> Int
size (VecN v) =
    List.length v
