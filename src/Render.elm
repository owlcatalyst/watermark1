module Render exposing (renderImage, renderText)

import Canvas
import Canvas.Settings exposing (fill)
import Canvas.Settings.Advanced exposing (alpha, rotate, scale, transform, translate)
import Canvas.Settings.Text as Text
import Color
import Model exposing (Model, Size, Watermark, WatermarkType(..), getDefaultFontSize)
import Parser exposing ((|.), (|=), Parser, chompIf, end, getChompedString, run, succeed, symbol)


renderImage : Model -> Watermark -> List Canvas.Renderable
renderImage model mark =
    let
        pos =
            vaildPos mark.position model.imageSize

        fontSize =
            Maybe.withDefault 1 (String.toFloat mark.fontSize)

        realSize =
            { width = mark.size.width * fontSize, height = mark.size.height * fontSize }

        image tex ( x, y ) ( centerX, centerY ) =
            Canvas.texture
                [ alpha <| toFloat mark.opacity / 100.0
                , transform
                    [ translate centerX centerY
                    , rotate (degrees mark.rotation)
                    , translate -centerX -centerY
                    , scale fontSize fontSize
                    ]
                ]
                ( x / fontSize, y / fontSize )
                tex
    in
    case mark.texture of
        Just tex ->
            if mark.tiled then
                createPattern model.imageSize mark (image tex)

            else
                [ image tex ( pos.x, pos.y ) ( pos.x + realSize.width / 2, pos.y + realSize.height / 2 ) ]

        Nothing ->
            []


renderText : Model -> Watermark -> List Canvas.Renderable
renderText model t =
    let
        pos =
            vaildPos t.position model.imageSize

        fontSize =
            Maybe.withDefault (round (getDefaultFontSize model.imageSize)) (String.toInt t.fontSize)

        color =
            case run hexToColor t.color of
                Ok val ->
                    val

                Err _ ->
                    Color.rgb255 0 0 0

        text ( x, y ) ( centerX, centerY ) =
            Canvas.text
                [ fill color
                , alpha <| toFloat t.opacity / 100.0
                , Text.font { size = fontSize, family = t.font }
                , Text.align Text.Center
                , transform
                    [ translate centerX centerY, rotate (degrees t.rotation), translate -centerX -centerY ]
                ]
                ( x, y )
                --坐标
                t.text
    in
    if t.tiled then
        createPattern model.imageSize t text

    else
        [ text ( pos.x, pos.y ) ( pos.x, pos.y - (toFloat fontSize / 2) ) ]



-- 考虑到旋转和移动，画n倍大（在有别的解决方法之前）


createPattern : Size -> Watermark -> (( Float, Float ) -> ( Float, Float ) -> Canvas.Renderable) -> List Canvas.Renderable
createPattern imageSize mark render =
    let
        vg =
            vaildGap mark.gap

        -- 实际计算的元素宽高
        ( fw, fh ) =
            case mark.type_ of
                Text ->
                    ( mark.size.width + vg.x, mark.size.height + vg.y )

                Image ->
                    ( mark.size.width * Maybe.withDefault 1 (String.toFloat mark.fontSize) + vg.x
                    , mark.size.height * Maybe.withDefault 1 (String.toFloat mark.fontSize) + vg.y
                    )

        n =
            toFloat <| ceiling <| max imageSize.width imageSize.height / min fw fh

        -- 绘制行列数
        row =
            List.range 1 (round (n*3))

        col =
            List.range 1 (round (n*3))

        -- 绘图中心点 旋转中心点
        hr : Int -> Int -> Canvas.Renderable
        hr a b =
            render
                ( toFloat a * fw - fw * n, toFloat b * fh - fh * n )
                ( imageSize.width / 2, imageSize.height / 2 )

        rowRender : Int -> List Canvas.Renderable
        rowRender a =
            List.map (\b -> hr a b) row
    in
    List.concatMap rowRender col



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
    { x = Maybe.withDefault (size.width / 2) (String.toFloat px)
    , y = Maybe.withDefault (size.height / 2) (String.toFloat py)
    }


vaildGap : ( String, String ) -> { x : Float, y : Float }
vaildGap ( gx, gy ) =
    { x = Maybe.withDefault 0 (String.toFloat gx)
    , y = Maybe.withDefault 0 (String.toFloat gy)
    }
