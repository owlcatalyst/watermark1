module Model exposing (..)

import Array exposing (Array)
import Canvas.Texture as Texture
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


initModel : List String -> Model
initModel fmts =
    { imageUrl = ""
    , imageTexture = Nothing
    , imageSize = { width = 0, height = 0 }
    , imageName = "未上传图片"
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
    , supportedFormat = fmts
    , log = "应用启动"
    , tempWatermark = Nothing
    }


initWatermark : WatermarkType -> String -> Size -> Watermark
initWatermark tp str size =
    let
        ( txt, tex, fsize ) =
            case tp of
                Text ->
                    ( str, "", "24" )

                Image ->
                    ( "", str, "1" )
    in
    { type_ = tp
    , text = txt
    , url = tex
    , texture = Nothing
    , size = { width = 0, height = 0 }
    , fontSize = fsize
    , tiled = False
    , color = "#000000"
    , opacity = 100
    , gap = ( "0", "0" )
    , font = "serif"
    , position = ( String.fromFloat (size.width / 2), String.fromFloat (size.height / 2) )
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
                            { width = width, height = Maybe.withDefault 24.0 (String.toFloat watermark.fontSize) }
                    in
                    { model | watermark = Just (Array.set model.selectedIndex { watermark | size = size } arr) }

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
            in
            (List.take (List.length list - 1) list |> List.foldl (++) "") ++ model.format.ext
    in
    E.object [ ( "format", E.string model.format.mime ), ( "name", E.string rename ) ]


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

supportedUploadFormat:List String 
supportedUploadFormat = 
    [ "image/jpeg", "image/png", "image/webp" ]