module UI.Color exposing (ColorType(..), hasBackground, hasText, is)

import Html.Attributes exposing (class)
import Html exposing (Attribute)

type ColorType
    = White
    | Black
    | Light
    | Dark
    | Primary
    | Link
    | Info
    | Success
    | Warning
    | Danger
    | PrimaryLight
    | LinkLight
    | InfoLight
    | SuccessLight
    | WarningLight
    | DangerLight


is : ColorType -> Attribute msg
is ct =
    case ct of
        White ->
            class "is-white"

        Black ->
            class "is-black"

        Light ->
            class "is-light"

        Dark ->
            class "is-dark"

        Primary ->
            class "is-primary"

        Link ->
            class "is-link"

        Info ->
            class "is-info"

        Success ->
            class "is-success"

        Warning ->
            class "is-warning"

        Danger ->
            class "is-danger"

        PrimaryLight ->
            class "is-primary is-light"

        LinkLight ->
            class "is-link is-light"

        InfoLight ->
            class "is-info is-light"

        SuccessLight ->
            class "is-success is-light"

        WarningLight ->
            class "is-warning is-light"

        DangerLight ->
            class "is-danger is-light"


hasText : ColorType -> Attribute msg
hasText ct =
    case ct of
        White ->
            class "has-text-white"

        Black ->
            class "has-text-black"

        Light ->
            class "has-text-light"

        Dark ->
            class "has-text-dark"

        Primary ->
            class "has-text-primary"

        Link ->
            class "has-text-link"

        Info ->
            class "has-text-info"

        Success ->
            class "has-text-success"

        Warning ->
            class "has-text-warning"

        Danger ->
            class "has-text-danger"

        PrimaryLight ->
            class "has-text-primary-light"

        LinkLight ->
            class "has-text-link-light"

        InfoLight ->
            class "has-text-info-light"

        SuccessLight ->
            class "has-text-success-light"

        WarningLight ->
            class "has-text-warning-light"

        DangerLight ->
            class "has-text-danger-light"


hasBackground : ColorType -> Attribute msg
hasBackground ct =
    case ct of
        White ->
            class "has-background-white"

        Black ->
            class "has-background-black"

        Light ->
            class "has-background-light"

        Dark ->
            class "has-background-dark"

        Primary ->
            class "has-background-primary"

        Link ->
            class "has-background-link"

        Info ->
            class "has-background-info"

        Success ->
            class "has-background-success"

        Warning ->
            class "has-background-warning"

        Danger ->
            class "has-background-danger"

        PrimaryLight ->
            class "has-background-primary-light"

        LinkLight ->
            class "has-background-link-light"

        InfoLight ->
            class "has-background-info-light"

        SuccessLight ->
            class "has-background-success-light"

        WarningLight ->
            class "has-background-warning-light"

        DangerLight ->
            class "has-background-danger-light"
