library(SVGAnnotation)
library(lattice)
library(XML)       

svg("axes_hrefs.svg", 10, 8)
plot(mpg ~ wt, mtcars, cex.lab = 2, cex.main = 2, main = "The title")
dev.off()

doc = xmlParse("axes_hrefs.svg")
addAxesLinks(doc, c("http://www.omegahat.org", "http://www.r-project.org",
                     "http://www.w3.org/Graphics/SVG"))

saveXML(doc, "axes_hrefs.svg")


##########################
doc = svgPlot(xyplot(y ~ x | f, data.frame(x = rnorm(100), y = rnorm(100), f = gl(5, 20)), main = "Conditional X-Y plot"))

addAxesLinks(doc, c("http://www.omegahat.org", "http://www.r-project.org",
                     "http://www.w3.org/Graphics/SVG"))
saveXML(doc, "xyplot.svg")
