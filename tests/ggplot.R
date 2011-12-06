if(FALSE && require(ggplot2)) {
 data(mtcars)
 library(SVGAnnotation)

 doc = svgPlot(qplot(factor(cyl), wt, geom=c("boxplot", "jitter"), data = mtcars))

 getPlotRegionNodes(doc)
}
