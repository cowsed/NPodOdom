module Mat exposing (Mat, rows, cols, num_rows, num_cols, from_rows, from_cols, transpose, mul)

import List.Extra
import Vec exposing (Vec)


type Mat n m
    = Mat (List (Vec m))


rows : Mat n m -> List (Vec m)
rows (Mat l) =
    l


cols : Mat n m -> List (Vec n)
cols m =
    m |> transpose |> rows


num_rows : Mat n m -> Int
num_rows m =
    rows m |> List.length


num_cols : Mat n m -> Int
num_cols m =
    cols m |> List.length


from_rows : List (Vec n) -> Mat m n
from_rows l =
    Mat l


from_cols : List (Vec n) -> Mat n m
from_cols l =
    Mat l |> transpose


transpose : Mat n m -> Mat m n
transpose (Mat vs) =
    List.map (\v -> Vec.coefs v) vs
        |> List.Extra.transpose
        |> List.map (\cs -> Vec.from_list cs)
        |> Mat


mul : Mat n m -> Mat m r -> Mat n r
mul a b =
    let
        r =
            num_cols b
    in
    List.Extra.cartesianProduct [ rows a, cols b ]
        |> List.map get2
        |> List.map (\( av, bv ) -> Vec.dot av bv)
        |> List.Extra.groupsOf r
        |> List.map Vec.from_list
        |> from_rows


get2 : List a -> ( a, a )
get2 l =
    case l of
        a :: l1 ->
            case List.head l1 of
                Just b ->
                    ( a, b )

                _ ->
                    Debug.todo "expected 2 fro get2 but wasnt 2"

        _ ->
            Debug.todo "expected 2 fro get2 but wasnt even 1"
