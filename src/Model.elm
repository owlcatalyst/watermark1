module Model exposing (..)

import Array exposing (Array)
import Canvas.Texture as Texture
import I18n exposing (Language(..), Translation, getTranslation, str2Lang)
import Json.Decode as D
import Json.Encode as E


type WatermarkType
    = Image
    | Text


type ImageType
    = Base
    | WatermarkImage


type BasicImageState
    = None
    | Loaded


type alias Size =
    { width : Float
    , height : Float
    }


type alias Point =
    { x : Float
    , y : Float
    }


type alias CalcPos =
    { x : Float
    , y : Float
    , imgw : Basics.Float
    , imgh : Float
    , canw : Float
    , canh : Float
    }


type DragState
    = Static
    | Moving


type alias Model =
    { imageUrl : String
    , imageTexture : Maybe Texture.Texture
    , imageSize : Size
    , imageName : String
    , imageState : BasicImageState
    , watermark : Maybe (Array Watermark)
    , watermarkInput : String
    , selectedIndex : Int
    , format : FormatData
    , fontCDN : String
    , supportedFormat : List String
    , log : String
    , tempWatermark : Maybe Watermark
    , clickPoint : Point
    , dragState : DragState
    , radio : Float
    , translations : Translation
    , lang : Language
    , base64Output : Bool
    }


type alias Watermark =
    { type_ : WatermarkType
    , text : String
    , url : String
    , texture : Maybe Texture.Texture
    , size : Size
    , fontSize : String
    , tiled : Bool
    , color : String
    , opacity : Int
    , gap : ( String, String )
    , font : String
    , position : ( String, String )
    , rotation : Float
    }


type alias Flag =
    { supportedFormats : List String
    , lang : String
    }


initModel : D.Value -> Model
initModel flag =
    let
        flagDecoder =
            D.map2 Flag
                (D.at [ "supportedFormats" ] (D.list D.string))
                (D.at [ "lang" ] D.string)

        base =
            { imageUrl = ""
            , imageTexture = Nothing
            , imageSize = { width = 0, height = 0 }
            , imageName = ""
            , imageState = None
            , watermark = Nothing
            , watermarkInput = ""
            , selectedIndex = -1
            , format =
                { name = "PNG"
                , ext = ".png"
                , mime = "image/png"
                }
            , fontCDN = ""
            , supportedFormat = []
            , log = ""
            , tempWatermark = Nothing
            , clickPoint = { x = 0, y = 0 }
            , dragState = Static
            , radio = 0
            , translations = getTranslation EN
            , lang = EN
            , base64Output = False
            }
    in
    case D.decodeValue flagDecoder flag of
        Ok f ->
            { base
                | supportedFormat = f.supportedFormats
                , log = (getTranslation (str2Lang f.lang)).log.appReady
                , translations = getTranslation (str2Lang f.lang)
                , lang = str2Lang f.lang
            }

        Err err ->
            { base | log = D.errorToString err }


initWatermark : WatermarkType -> String -> Size -> Watermark
initWatermark tp str size =
    case tp of
        Text ->
            { type_ = tp
            , text = str
            , url = ""
            , texture = Nothing
            , size = { width = 0, height = 0 }
            , fontSize = String.fromFloat (getDefaultFontSize size)
            , tiled = False
            , color = "#000000"
            , opacity = 100
            , gap = ( "0", "0" )
            , font = "serif"
            , position = ( String.fromFloat (size.width / 2), String.fromFloat (size.height / 2) )
            , rotation = 0.0
            }

        Image ->
            { type_ = tp
            , text = ""
            , url = str
            , texture = Nothing
            , size = { width = 0, height = 0 }
            , fontSize = "1"
            , tiled = False
            , color = "#000000"
            , opacity = 100
            , gap = ( "0", "0" )
            , font = "serif"
            , position = ( "0", "0" )
            , rotation = 0.0
            }



-- 处理Update


updateWatermark : Watermark -> Model -> Model
updateWatermark watermark model =
    case model.watermark of
        Just arr ->
            case Array.get model.selectedIndex arr of
                Just _ ->
                    { model | watermark = Just (Array.set model.selectedIndex watermark arr) }

                Nothing ->
                    model

        Nothing ->
            model


updateTextSize : String -> Model -> Model
updateTextSize number model =
    case model.watermark of
        Just arr ->
            case Array.get model.selectedIndex arr of
                Just watermark ->
                    { model | watermark = Just (Array.set model.selectedIndex { watermark | fontSize = number } arr) }

                Nothing ->
                    model

        Nothing ->
            model



-- 不能让输入的过程被打断，用String格式，不然修改输入很烦人


updateTextPositon : ( String, String ) -> Model -> Model
updateTextPositon ( x, y ) model =
    case model.watermark of
        Just arr ->
            case Array.get model.selectedIndex arr of
                Just watermark ->
                    { model | watermark = Just (Array.set model.selectedIndex { watermark | position = ( String.trim x, String.trim y ) } arr) }

                Nothing ->
                    model

        Nothing ->
            model


updateSize : Float -> Model -> Model
updateSize width model =
    case model.watermark of
        Just arr ->
            case Array.get model.selectedIndex arr of
                Just watermark ->
                    let
                        size =
                            { width = width, height = Maybe.withDefault (getDefaultFontSize model.imageSize) (String.toFloat watermark.fontSize) }
                    in
                    { model | watermark = Just (Array.set model.selectedIndex { watermark | size = size } arr) }

                Nothing ->
                    model

        Nothing ->
            model


updatePosition : Point -> Model -> Model
updatePosition pos model =
    case model.watermark of
        Just arr ->
            case Array.get model.selectedIndex arr of
                Just watermark ->
                    let
                        ( x, y ) =
                            watermark.position

                        newX =
                            String.fromFloat <| Maybe.withDefault 0 (String.toFloat x) + ((-model.clickPoint.x + pos.x) * model.radio)

                        newY =
                            String.fromFloat <| Maybe.withDefault 0 (String.toFloat y) + ((-model.clickPoint.y + pos.y) * model.radio)
                    in
                    if watermark.tiled == False then
                        { model | watermark = Just (Array.set model.selectedIndex { watermark | position = ( newX, newY ) } arr), clickPoint = { x = pos.x, y = pos.y } }

                    else
                        { model | clickPoint = { x = pos.x, y = pos.y } }

                Nothing ->
                    model

        Nothing ->
            model



-- 文字数据生成


encodeText : Watermark -> E.Value
encodeText wm =
    E.object <|
        [ ( "text", E.string wm.text )
        , ( "fontsize", E.string wm.fontSize )
        , ( "font", E.string wm.font )
        ]


encodeImage : Model -> E.Value
encodeImage model =
    let
        rename =
            let
                list =
                    String.split "." model.imageName

                name =
                    List.take (List.length list - 1) list |> List.foldl (++) ""
            in
            if model.base64Output then
                name ++ "-" ++ String.dropLeft 1 model.format.ext ++ "-base64.txt"

            else
                name ++ model.format.ext
    in
    E.object [ ( "format", E.string model.format.mime ), ( "name", E.string rename ), ( "base64", E.bool model.base64Output ) ]


printSize : Size -> String
printSize size =
    "(" ++ String.fromFloat size.width ++ "x" ++ String.fromFloat size.height ++ ")"


type alias FormatData =
    { name : String
    , ext : String
    , mime : String
    }


formatData : List FormatData
formatData =
    [ { name = "WebP"
      , ext = ".webp"
      , mime = "image/webp"
      }
    , { name = "PNG"
      , ext = ".png"
      , mime = "image/png"
      }
    , { name = "JPG"
      , ext = ".jpg"
      , mime = "image/jpeg"
      }
    , { name = "BMP"
      , ext = ".bmp"
      , mime = "image/bmp"
      }
    ]


supportedUploadFormat : List String
supportedUploadFormat =
    [ "image/jpeg", "image/png", "image/webp", "image/bmp" ]


getDefaultFontSize : Size -> Float
getDefaultFontSize size =
    size.width / 15
