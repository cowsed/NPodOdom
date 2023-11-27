module MatN exposing (..)

import List.Extra
import VecN exposing (VecN)


type MatN
    = MatN (List VecN)


add : MatN -> MatN -> MatN
add (MatN arows) (MatN brows) =
    List.Extra.zip arows brows |> List.map (\( a, b ) -> VecN.add a b) |> MatN


mul : MatN -> MatN -> MatN
mul a b =
    let
        m =
            get_num_rows a

        n1 =
            get_num_cols a

        n2 =
            get_num_rows b

        r =
            get_num_cols b

        multiplyable =
            if n1 == n2 then
                True 
            else
                Debug.todo ("2 Matrices not multiplyable: " ++ String.fromInt m ++ " by " ++ String.fromInt n1 ++ " times " ++ String.fromInt n2 ++ " by " ++ String.fromInt r)
    in
    List.Extra.cartesianProduct [(rows a ), (cols b  )]
        |> Debug.log "zipped"
        |> List.map get2
        |> List.map (\( av, bv ) -> VecN.dot av bv)
        |> from_list m r
get2 : List a ->  ( a, a )
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


from_list : Int -> Int -> List Float -> MatN
from_list nrows ncols coefs =
    coefs
        |> Debug.log "from list coefs"
        |> List.Extra.groupsOf ncols
        |> Debug.log "groups"
        |> List.map VecN.from_list
        |> MatN
        |> assert_size nrows ncols


from_rows : List VecN -> MatN
from_rows l =
    MatN l


transpose : MatN -> MatN
transpose (MatN vs) =
    List.map (\v -> VecN.coefs v) vs
        |> Debug.log "transposing"
        |> List.Extra.transpose
        |> List.map (\cs -> VecN.from_list cs)
        |> MatN


transpose_list : List (List a) -> List (List a)
transpose_list ll =
    let
        heads =
            List.map (List.take 1) ll |> List.concat

        tails =
            List.map (List.drop 1) ll
    in
    if List.isEmpty heads then
        []

    else
        heads :: transpose_list tails


assert_size : Int -> Int -> MatN -> MatN
assert_size nrows ncols m =
    let
        thing =
            Debug.log "asserting size" m

        rowsgood =
            assert_same nrows (get_num_rows m) ("rows should equal expected rows" ++ (String.fromInt nrows ++ "!=" ++ String.fromInt (get_num_rows m)))

        colsgood =
            assert_same ncols (get_num_cols m) "rows should equal expected cols"
    in
    m


assert_same : Int -> Int -> String -> Bool
assert_same a b msg =
    if a == b then
        True

    else
        Debug.todo msg


rows : MatN -> List VecN
rows (MatN l) =
    l


cols : MatN -> List VecN
cols mat =
    transpose mat |> rows


get_num_rows : MatN -> Int
get_num_rows (MatN a) =
    List.length a


get_num_cols : MatN -> Int
get_num_cols (MatN a) =
    case List.head a of
        Just v ->
            VecN.size v

        Nothing ->
            0
