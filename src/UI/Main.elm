module UI.Main exposing (..)

import Html exposing (Attribute, Html, div, h1, span, text)
import Html.Attributes as Attributes exposing (attribute, class, style)
import Html.Events exposing (onClick)
import UI.Icons as Icons



--Elements---------------------------------------------------------------------------------------
-- Buttons


button : List (Attribute msg) -> List (Html msg) -> Html msg
button attr list =
    Html.button (List.append [ class "button" ] attr) list


iconButton : Html msg -> List (Attribute msg) -> List (Html msg) -> Html msg
iconButton icon attribute msg =
    button attribute (List.append [ span [] [ icon ] ] msg)


notification : List (Attribute msg) -> List (Html msg) -> Html msg
notification attr msg =
    div (List.append [ class "notification" ] attr) msg


uploadFrame : Html msg
uploadFrame =
    div [ class "upload-frame has-text-primary" ]
        [ Icons.image
        , span [] [ text "点击上传或拖拽文件" ]
        ]


progress : List (Attribute msg) -> Maybe Int -> Html msg
progress attr percent =
    case percent of
        Just p ->
            div (List.append [ class "control", Attributes.value (String.fromInt p), Attributes.max "100" ] attr) [ text (String.fromInt p ++ " %") ]

        Nothing ->
            div (List.append [ class "control", Attributes.max "100" ] attr) []



-- Title


title : List (Attribute msg) -> List (Html msg) -> Html msg
title attr list =
    h1 (List.append [ class "title" ] attr) list



-- Use bulma-tooltip


tooltip : String -> List (Attribute msg) -> List (Html msg) -> Html msg
tooltip tip attr list =
    span (List.append [ class "has-tooltip-arrow", Attributes.attribute "data-tooltip" tip ] attr) list



--Columns---------------------------------------------------------------------------------------


columns : List (Attribute msg) -> List (Html msg) -> Html msg
columns attr list =
    div (List.append [ class "columns" ] attr) <| list


column : List (Attribute msg) -> List (Html msg) -> Html msg
column attribute list =
    div (List.append [ class "column " ] attribute) <| list



--Layout---------------------------------------------------------------------------------------


section : List (Attribute msg) -> List (Html msg) -> Html msg
section attr list =
    Html.section (List.append [ class "section" ] attr) list


container : List (Attribute msg) -> List (Html msg) -> Html msg
container attr list =
    div (List.append [ class "container" ] attr) list


footer : List (Attribute msg) -> List (Html msg) -> Html msg
footer attr list =
    Html.footer (List.append [ class "footer" ] attr) list



--Form---------------------------------------------------------------------------------------


field : List (Attribute msg) -> List (Html msg) -> Html msg
field attr list =
    div (List.append [ class "field" ] attr) list


control : List (Attribute msg) -> List (Html msg) -> Html msg
control attr list =
    div (List.append [ class "control" ] attr) list


label : List (Attribute msg) -> List (Html msg) -> Html msg
label attr list =
    Html.label (List.append [ class "label" ] attr) list


fieldAddon : List (Attribute msg) -> List (Html msg) -> Html msg
fieldAddon attr a =
    div (List.append [ class "field has-addons" ] attr) <|
        List.map (\t -> control [] [ t ]) a


fieldGroup : List (Attribute msg) -> List (Html msg) -> Html msg
fieldGroup attr a =
    div (List.append [ class "field is-grouped" ] attr) <|
        List.map (\t -> control [] [ t ]) a


fieldHorizontal : List (Attribute msg) -> String -> List (Html msg) -> Html msg
fieldHorizontal attr lb list =
    let
        fieldLable =
            div [ class "field-label" ] [ label [] [ text lb ] ]

        fieldBody =
            div [ class "field-body" ] <| List.map (\t -> field [] [ t ]) list
    in
    div (List.append [ class "field is-horizontal" ] attr) <| [ fieldLable, fieldBody ]


input : List (Attribute msg) -> List (Html msg) -> Html msg
input attr list =
    Html.input (List.append [ class "input", Attributes.type_ "text" ] attr) list


textarea : List (Attribute msg) -> List (Html msg) -> Html msg
textarea attr list =
    Html.textarea (List.append [ class "textarea"] attr) list



{-
   size：显示的高度个数
   selected: 根据Index判断是否被选中
   msg： onclick发出消息
-}


selectMultiple : Int -> List (Attribute msg) -> List String -> (Int -> Bool) -> (Int -> msg) -> Html msg
selectMultiple size attr list selected msg =
    div (List.append [ class "select is-multiple" ] attr)
        [ Html.select [ Attributes.multiple True, Attributes.size size, style "width" "100%" ] <|
            List.indexedMap (\num -> \t -> Html.option [ Attributes.value t, onClick (msg num), Attributes.selected (selected num) ] [ text t ]) list
        ]


select : List (Attribute msg) -> List (Html msg) -> Html msg
select attr list =
    div (List.append [ class "select" ] attr) [ Html.select [] list ]



-- Use Bulma-slider


slider : Int -> Int -> Int -> List (Attribute msg) -> List (Html msg) -> Html msg
slider min max value attr list =
    Html.input
        (List.append
            [ class "slider is-fullwidth"
            , Attributes.step "1"
            , Attributes.min (String.fromInt min)
            , Attributes.max (String.fromInt max)
            , Attributes.value (String.fromInt value)
            , Attributes.type_ "range"
            ]
            attr
        )
        list



-- Use bulma-checkradio


checkbox : Bool -> String -> List (Attribute msg) -> Html msg
checkbox b str attr =
    field [] [ Html.input [ class "is-checkradio", Attributes.type_ "checkbox", Attributes.checked b ] [], label attr [ text str ] ]


radio : List (Attribute msg) -> List (Attribute msg) -> List (Html msg) -> Html msg
radio outAttr attr list =
    span outAttr (List.append [ Html.input (List.append [ class "is-checkradio", Attributes.type_ "radio" ] attr) [] ] list)



--Components---------------------------------------------------------------------------------------
--单纯的card


card : List (Attribute msg) -> List (Html msg) -> Html msg
card attr list =
    div (List.append [ class "card" ] attr)
        [ div [ class "card-content" ] list
        ]


modal : List (Attribute msg) -> List (Html msg) -> msg -> Bool -> Html msg
modal attr list msg isOpen =
    let
        display =
            if isOpen then
                "block"

            else
                "none"
    in
    div [ class "modal", style "display" display ]
        [ div [ class "modal-background" ]
            [ div
                (List.append
                    [ class "modal-content"
                    , style "margin" "0 auto"
                    , style "position" "relative"
                    , style "top" "50vh"
                    , style "transform" "translateY(-50%)"
                    ]
                    attr
                )
                list
            , Html.button [ class "modal-close is-large", attribute "aria-label" "close", onClick msg ] []
            ]
        ]
