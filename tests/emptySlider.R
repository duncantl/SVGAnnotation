library(XML)
library(SVGAnnotation)

f = 'empty.svg'
svg(f, 10, 10)
plot(1, type = "n", axes = FALSE)
dev.off()

doc = xmlParse(f)
svg = xmlRoot(doc)

enlargeSVGViewBox(doc, y = 100, svg = svg)

newXMLNode("g", attrs = c(id = "slider-alpha"), parent = svg)
newXMLNode("g", attrs = c(id = "slider-beta"), parent = svg)

addAttributes(svg, onload = "init(evt);")

init = c("",
         ' var myMapApp = new mapApp(false,undefined);',
         ' var slider1;',
         ' var slider2;',
         "",
         "function init(evt) {",
         '   var sliderStyles={"stroke":"dimgray","stroke-width":3};',
         '   var invisSliderWidth = 15;',
         '   slider1 = new slider("slider-alpha", "slider-alpha", 100, 510, 0, 475, 510, 100, 50, sliderStyles, invisSliderWidth, "sliderSymbol", showVal, true);',
         '   slider2 = new slider("slider-beta", "slider-beta", 100, 560, 0, 475, 560, 100, 50, sliderStyles, invisSliderWidth, "sliderSymbol", showVal, true);',  
         "}",
         "",
         "function showVal(type, group, value) {",
         '  // alert(type + " " + group + " " + value);',
         "}")

addECMAScripts(doc, c("mapApp.js", "helper_functions.js", "slider.js"), FALSE)
addECMAScripts(doc, I(paste(init, collapse = "\n")), TRUE)

addCSS(doc)

defs = getNodeSet(doc, "//x:defs", "x")[[1]]

newXMLNode("symbol", attrs = c(id = "sliderSymbol",  overflow = "visible"),
	    newXMLNode("line", attrs = c(x1 = "0",  y1 = "-10",  x2 = "0",  y2 = "10",  stroke = "dimgray",
                           'stroke-width' = "5", 'pointer-events' = "none")),
            parent = defs)

saveXML(doc, docName(doc))

