# This illustrates a simple legend within a scatterplot
library(SVGAnnotation)
svgPlot({
x = runif(100)
y = runif(100)
type = sample(c("A", "B"), 100, replace = TRUE)
plot(x,y, ylim = c(0, 1.2),
     col = c("red", "green")[factor(type)], pch = c(21, 23)[factor(type)])
legend(.5, 1.2, c("Male", "Female"), col = c("red", "green"), pch = c(21, 23), merge = FALSE)
}, "legend.svg")


doc = xmlParse("legend.svg")
a = getAxesLabelNodes(doc)
getNodeSet(xmlParent(a[[1]]), "./following-sibling::*")
# This is a <g> element with
#  path, path, path, g, g
# The three paths are the outer rectangle, and the plotting characters.
# The two g's are the labels.
#
# So we can easily add an onmouseover attribute to the labels.

