//test supported format
const formats = ["image/png", "image/bmp", "image/webp", "image/jpeg"];
const supportFormats = [];
const canvas0 = document.createElement("canvas");
formats.forEach((fmt) => {
  if (canvas0.toDataURL(fmt).split(/[;:]/)[1].trim() == fmt)
    supportFormats.push(fmt);
});
//浏览器使用语言
const currentLang = navigator.language;

//init
var app = Elm.Main.init({
  node: document.getElementById("app"),
  flags: { supportedFormats: supportFormats, lang: currentLang },
});
//-----保存图片
app.ports.saveImage.subscribe(function (message) {
  const canvas = document.getElementsByTagName("canvas")[0];
  if (canvas === undefined) return;

  //建立blob
  const download = document.createElement("a");
  download.style.visibility = "hidden";

  if (message.base64) {
    download.href =
      "data:text/plain;charset=utf-8," +
      encodeURIComponent(canvas.toDataURL(message.format, 1));
  } else {
    download.href = canvas.toDataURL(message.format, 1);
  }

  download.download = message.name;

  document.body.appendChild(download);
  let e = document.createEvent("MouseEvents");
  e.initEvent("click", true, true);
  download.dispatchEvent(e);

  document.body.removeChild(download);
});

//-----测量文字宽度，毕竟非等宽
app.ports.measureText.subscribe(function (message) {
  const context = document.getElementsByTagName("canvas")[0].getContext("2d");
  if (context === undefined) return;
  const fontStyle = `${message.fontsize}px "${message.font}"`;
  context.font = fontStyle;

  let width = context.measureText(message.text).width;
  app.ports.receiveTextWidth.send(width);
});
