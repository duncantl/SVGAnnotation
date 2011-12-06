library(SVGAnnotation)
if(require(vcd)) {
  data(Titanic)
  mos = svgPlot(m <- mosaic(~ Sex + Age + Survived, data = Titanic,
                 main = "Survival on the Titanic", shade = TRUE, legend = TRUE))
  annotateMosaic(mos, m)
  saveXML(mos, "titanicMosaic.svg")
}
