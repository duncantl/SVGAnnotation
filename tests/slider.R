#
#  Plot a beta distributions and add two sliders
#  When we move one, we hide a different plot.
#

# Todo:
#   put the min and the max of the alpha and beta beside the slider.
#   place the statusText more appropriately, perhaps split it into 2.
#    or put a new line.
#  Done put greek symbols alpha and beta in the status message
#

library(SVGAnnotation)

alpha = seq(.01, by = 0.1, length = 30)
beta = seq(.01, by = 0.1, length = 30)

grid = expand.grid(alpha, beta)

f = 'beta.svg'
svg(f)
plot(0, type = "n", xlim = c(0, 1), ylim = c(0, 2.5), xlab = "X", ylab = "density",
         main = "Density of beta distribution")
apply(grid, 1, function(p) curve(dbeta(x, p[1], p[2]), 0, 1, n = 300, add = TRUE))
#curve(dbeta(x, .3, .7), 0, 1, add = TRUE)
#curve(dbeta(x, .7, .3), 0, 1, add = TRUE, col = "red")
dev.off()

#######
# Now post-process.

library(XML)
library(SVGAnnotation)

doc = xmlParse(f)

box = getViewBox(doc)

p = getPlotRegionNodes(doc)[[1]]

#plot.box = getPlotRects(doc)[[1]]
#w = xmlSApply(p[[1]], function(x) pathInBox(as(x, "SVGPath"), plot.box))
#w = xmlSApply(p[[1]], function(x) nrow(as(x, "SVGPath"))) > 10

#ids = paste("curve", grid[,1]*100, grid[,2]*100, sep = "-")
id.grid = expand.grid(seq(along = alpha), seq(along = beta))
ids = paste("curve", id.grid[,1], id.grid[,2], sep = "-")
invisible(
          sapply(seq(along = ids),
                 function(i)
                     addAttributes(p[[i]], .attrs = c(id = ids[i], visibility = "hidden",
                                                      alpha = grid[i,1], beta = grid[i, 2]))))

addAttributes(p[[1]], .attrs = c(visibility = "visible"))


##########

svg = xmlRoot(doc)

invisible(enlargeSVGViewBox(doc, y = 100, svg = svg))

newXMLNode("g", attrs = c(id = "slider-alpha"), parent = svg)
newXMLNode("g", attrs = c(id = "slider-beta"), parent = svg)

newXMLNode("text",  attrs = c(x = "5",  y = box[2, 2] - 10, id = "statusText"), "<>", parent = svg)

invisible(addAttributes(svg, onload = sprintf("init(evt, %d, %d);", length(alpha), length(beta))))

addECMAScripts(doc, c("mapApp.js", "helper_functions.js", "slider.js", "betaSlider.js"), TRUE)
#addECMAScripts(doc, I(paste(init, collapse = "\n")), TRUE)

addCSS(doc)

defs = getNodeSet(doc, "//x:defs", "x")[[1]]

newXMLNode("symbol", attrs = c(id = "sliderSymbol",  overflow = "visible"),
	    newXMLNode("line", attrs = c(x1 = "0",  y1 = "-10",  x2 = "0",  y2 = "10",
                                         stroke = "dimgray", 'stroke-width' = "5",
                                         'pointer-events' = "none")),
            parent = defs)

saveXML(doc, docName(doc))
