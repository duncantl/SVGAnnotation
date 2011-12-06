# From examples hexbin
library(SVGAnnotation)
if(require(hexbin)) {
set.seed(101)
x <- rnorm(10000)
y <- rnorm(10000)
hbin <- hexbin(x, y + x*(x+1)/4)
doc = svgPlot(plot(hbin, main = "(X, X(X+1)/4 + Y)  where X,Y ~ rnorm(10000)"))

# hbin is an S4 object.

#############Annotation

#doc = xmlParse("hexbin.svg")
 # Just one.
r = getPlotRegionNodes(doc)[[1]]

kids = xmlChildren(r)
sapply(seq(along = kids),
       function(i) {
         addToolTips(kids[[i]], paste(hbin@count[i], round(hbin@xcm[i], 4), round(hbin@ycm[i], 4)), addArea = 2)
       })
saveXML(doc, "hexbin_tt.svg")


# Exercise: Do for lattice
#  Allow the user to click on an element in the legend and highlight the corresponding
#  polygon borders.
#  The legend can be found file getNodeSet(getPlotRegionNodes()[[last]], "./following-sibling::*")


}
