module Render exposing (renderImage, renderText)

import Canvas
import Canvas.Settings exposing (fill)
import Canvas.Settings.Advanced exposing (alpha, rotate, scale, transform, translate)
import Canvas.Settings.Text as Text
import Color
import Model exposing (Model, Size, Watermark)
import Parser exposing ((|.), (|=), Parser, chompIf, end, getChompedString, run, succeed, symbol)
import Svg.Attributes exposing (fontSize)


renderImage : Model -> Watermark -> List Canvas.Renderable
renderImage model mark =
    let
        pos =
            vaildPos mark.position model.imageSize

        fontSize =
            Maybe.withDefault 1 (String.toFloat mark.fontSize)

        realSize =
            { width = mark.size.width * fontSize, height = mark.size.height * fontSize }

        ( transX, transY ) =
            ( (pos.x + realSize.width / 2) / fontSize
            , (pos.y + realSize.height / 2) / fontSize
            )

        image tex ( x, y ) =
            Canvas.texture
                [ alpha <| toFloat mark.opacity / 100.0
                , transform
                    [ scale fontSize fontSize
                    , translate transX transY
                    , rotate (degrees mark.rotation)
                    , translate -transX -transY
                    ]
                ]
                ( x / fontSize, y / fontSize )
                tex
    in
    case mark.texture of
        Just tex ->
            if mark.tiled then
                createPattern realSize model.imageSize mark.gap (image tex)

            else
                [ image tex ( pos.x, pos.y ) ]

        Nothing ->
            []


renderText : Model -> Watermark -> List Canvas.Renderable
renderText model t =
    let
        pos =
            vaildPos t.position model.imageSize

        fontSize =
            Maybe.withDefault 24 (String.toInt t.fontSize)

        color =
            case run hexToColor t.color of
                Ok val ->
                    val

                Err _ ->
                    Color.rgb255 0 0 0

        text ( c, d ) =
            Canvas.text
                [ fill color
                , alpha <| toFloat t.opacity / 100.0
                , Text.font { size = fontSize, family = t.font }
                , Text.align Text.Center
                , transform
                    [ translate pos.x pos.y, rotate (degrees t.rotation), translate -pos.x -pos.y ]
                ]
                ( c, d )
                --坐标
                t.text
    in
    if t.tiled then
        createPattern t.size model.imageSize t.gap text

    else
        [ text ( pos.x, pos.y ) ]



-- 考虑到旋转和移动，画4倍大（在有别的解决方法之前）


createPattern : Size -> Size -> ( String, String ) -> (( Float, Float ) -> Canvas.Renderable) -> List Canvas.Renderable
createPattern psize imagesize gap render =
    let
        vg =
            vaildGap gap

        -- 实际计算的元素宽
        fw =
            psize.width + vg.x

        -- 实际计算的元素高
        fh =
            psize.height + vg.y

        hr : Int -> Int -> Canvas.Renderable
        hr a b =
            render ( toFloat a * fw - imagesize.width / 2, toFloat b * fh - imagesize.height / 2 )

        heightRender : Int -> List Canvas.Renderable
        heightRender a =
            List.map (\b -> hr a b) (List.range 1 (ceiling (imagesize.height * 2 / fh)))
    in
    List.concatMap heightRender (List.range 1 (ceiling (imagesize.width * 2 / fw)))



-- Hex到颜色


hexToColor : Parser Color.Color
hexToColor =
    let
        hexToInt : Parser Int
        hexToInt =
            Parser.map intFromHexString <|
                getChompedString <|
                    chompIf Char.isHexDigit
                        |. chompIf Char.isHexDigit
    in
    succeed Color.rgb255
        |. symbol "#"
        |= hexToInt
        |= hexToInt
        |= hexToInt
        |. end


intFromHexString : String -> Int
intFromHexString hex =
    let
        singleHex c =
            if Char.isDigit c then
                Char.toCode c - Char.toCode '0'

            else
                Char.toCode c - Char.toCode 'A' + 10

        power i =
            List.product (List.repeat i 16)
    in
    String.toList (String.toUpper hex)
        |> List.indexedMap (\i item -> power (String.length hex - i - 1) * singleHex item)
        |> List.sum


vaildPos : ( String, String ) -> Size -> { x : Float, y : Float }
vaildPos ( px, py ) size =
    { x = clamp 0.0 size.width (Maybe.withDefault (size.width / 2) (String.toFloat px))
    , y = clamp 0.0 size.height (Maybe.withDefault (size.height / 2) (String.toFloat py))
    }


vaildGap : ( String, String ) -> { x : Float, y : Float }
vaildGap ( gx, gy ) =
    { x = Maybe.withDefault 0 (String.toFloat gx)
    , y = Maybe.withDefault 0 (String.toFloat gy)
    }
