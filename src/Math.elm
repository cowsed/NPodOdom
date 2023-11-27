module Math exposing (..)

import Html
import Html.Attributes exposing (style)
import MathML as Mt
import MathML.Attributes as Mta

type Op
    = Add
    | Sub
    | Mul
    | Div
    | CustomOp String


type Node
    = Number Float
    | Symbol String
    | Parenthesized Node
    | SubScript Node Node
    | SuperScript Node Node
    | Infix Op Node Node
    | Sqrt Node


tabs: Int -> Html.Html msg
tabs i = String.repeat i "    " |> Html.text


render : Node -> Html.Html msg
render n =
    Mt.math [] [render_node n]



pythag = 
    SuperScript (Symbol "a") (Number 2)


-- mfenced: Html.Html msg -> Html.Html msg
-- mfenced


mo : Op -> Html.Html msg
mo o =
    Html.node "mo" [] [ Html.text <| string_op o ]


string_op : Op -> String
string_op op =
    case op of
        Add ->
            "+"

        Sub ->
            "-"

        Mul ->
            "×"

        Div ->
            "/"

        CustomOp s ->
            s


render_node : Node -> Html.Html msg
render_node n =
    case n of
        Symbol s ->
            Mt.mi [] [Html.text s]

        Number f ->
            Mt.mn [] [Html.text (String.fromFloat f)]

        Parenthesized p ->
            render_node p

        SubScript _ _->
            Debug.todo "branch 'SubScript _' not implemented"

        SuperScript below above->
            Mt.msup [] [render_node below, render_node above]


        Infix op a b ->
            Html.node "mrow" [] [ render_node a, mo op, render_node b ]

        Sqrt exp ->
            Html.node "msqrt" [] [ render_node exp ]
