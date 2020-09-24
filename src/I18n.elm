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
    }


type alias Button =
    { textWatermark : String
    , imageWatermark : String
    , deleteWatermark : String
    , removeBaseImage : String
    , save : String
    , clickToUpload : String
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
        }
    , button =
        { textWatermark = "文字"
        , imageWatermark = "图片"
        , deleteWatermark = "删除水印"
        , removeBaseImage = "移除当前图片"
        , save = "保存"
        , clickToUpload = "点击上传"
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
        }
    , button =
        { textWatermark = "Text"
        , imageWatermark = "Image"
        , deleteWatermark = "Delete"
        , removeBaseImage = "Remove Image"
        , save = "Save"
        , clickToUpload = "Click to upload"
        }
    , menu =
        { watermarkList = "Watermark List"
        , inputPlaceHolder = "Input Text ..."
        , outputFormat = "Output format"
        , doNotSupport = "Format not supported by the browser."
        , fontCDN = "Font CDN"
        , language = "Language"
        }
    , log =
        { loadTextureFailed = "Failed to load texture."
        , noImage = "Image not uploaded."
        , appReady = "Ready."
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
