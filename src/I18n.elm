module I18n exposing (Language(..), Translation, getLanguageList, getTranslation, str2Lang)


type Language
    = ZH
    | EN


type alias Translation =
    { label : Label
    , button : Button
    , menu : Menu
    , log : Log
    , self : String
    , lang : Language
    , blendMode : BlendMode
    }


type alias Label =
    { rotate : String
    , coordinate : String
    , color : String
    , font : String
    , opacity : String
    , content : String
    , size : String
    , tile : String
    , spacing : String
    , blendMode : String
    }


type alias Button =
    { textWatermark : String
    , imageWatermark : String
    , deleteWatermark : String
    , removeBaseImage : String
    , save : String
    , clickToUpload : String
    , reset:String
    }


type alias Menu =
    { watermarkList : String
    , inputPlaceHolder : String
    , outputFormat : String
    , doNotSupport : String
    , fontCDN : String
    , language : String
    }


type alias Log =
    { loadTextureFailed : String
    , noImage : String
    , appReady : String
    }


type alias BlendMode =
    { sourceOver : String

    -- 省略
    , multiply : String
    , screen : String
    , overlay : String
    , darken : String
    , lighten : String
    , colorDodge : String
    , colorBurn : String
    , hardLight : String
    , softLight : String
    , difference : String
    , exclusion : String
    , hue : String
    , saturation : String
    , color : String
    , luminosity : String
    }


zh : Translation
zh =
    { self = "中文"
    , lang = ZH
    , label =
        { rotate = "旋转"
        , coordinate = "坐标"
        , color = "颜色"
        , font = "字体"
        , opacity = "透明"
        , content = "内容"
        , size = "尺寸"
        , tile = "平铺"
        , spacing = "间距"
        , blendMode = "混合模式"
        }
    , button =
        { textWatermark = "文字"
        , imageWatermark = "图片"
        , deleteWatermark = "删除水印"
        , removeBaseImage = "移除当前图片"
        , save = "保存"
        , clickToUpload = "点击上传"
        , reset="重置"
        }
    , menu =
        { watermarkList = "水印列表"
        , inputPlaceHolder = "请输入文字水印……"
        , outputFormat = "输出格式"
        , doNotSupport = "您的浏览器不支持转换该格式"
        , fontCDN = "字体 CDN"
        , language = "语言"
        }
    , log =
        { loadTextureFailed = "纹理加载失败"
        , noImage = "未上传图片"
        , appReady = "应用准备就绪"
        }
    , blendMode =
        { sourceOver = "默认"
        , multiply = "正片叠底"
        , screen = "滤色"
        , overlay = "叠加"
        , darken = "变暗"
        , lighten = "变亮"
        , colorDodge = "颜色减淡"
        , colorBurn = "颜色加深"
        , hardLight = "强光"
        , softLight = "柔光"
        , difference = "插值"
        , exclusion = "排除"
        , hue = "色调"
        , saturation = "饱和度"
        , color = "颜色"
        , luminosity = "亮度"
        }
    }


en : Translation
en =
    { self = "English"
    , lang = EN
    , label =
        { rotate = "Rotate"
        , coordinate = "Coordinate"
        , color = "Color"
        , font = "Font"
        , opacity = "Opacity"
        , content = "Content"
        , size = "Size"
        , tile = "Tile"
        , spacing = "Spacing"
        , blendMode = "Blend Mode"
        }
    , button =
        { textWatermark = "Text"
        , imageWatermark = "Image"
        , deleteWatermark = "Delete"
        , removeBaseImage = "Remove Image"
        , save = "Save"
        , clickToUpload = "Click to upload"
        , reset = "Reset"
        }
    , menu =
        { watermarkList = "Watermark List"
        , inputPlaceHolder = "Input Text ..."
        , outputFormat = "Output Format"
        , doNotSupport = "Format not supported by the browser."
        , fontCDN = "Font CDN"
        , language = "Language"
        }
    , log =
        { loadTextureFailed = "Failed to load texture."
        , noImage = "Image not uploaded."
        , appReady = "Ready."
        }
    , blendMode =
        { sourceOver = "Normal"
        , multiply = "Multiply"
        , screen = "Screen"
        , overlay = "Overlay"
        , darken = "Darken"
        , lighten = "Lighten"
        , colorDodge = "Color Dodge"
        , colorBurn = "Color Burn"
        , hardLight = "Hard Light"
        , softLight = "Soft Light"
        , difference = "Difference"
        , exclusion = "Exclusion"
        , hue = "Hue"
        , saturation = "Saturation"
        , color = "Color"
        , luminosity = "Luminosity"
        }
    }


getTranslation : Language -> Translation
getTranslation lang =
    case lang of
        ZH ->
            zh

        EN ->
            en


str2Lang : String -> Language
str2Lang str =
    case List.head (String.split "-" str) of
        Just "zh" ->
            ZH

        Just "en" ->
            EN

        _ ->
            EN


getLanguageList : List { text : String, lang : Language }
getLanguageList =
    List.map (\i -> { text = i.self, lang = i.lang }) [ zh, en ]
