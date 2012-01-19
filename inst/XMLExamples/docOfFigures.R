library(XML)
doc = xmlParse("~/Books/XMLTechnologies/SVGAnnotation/SVGAnnotationPaper.xml")

figs = getNodeSet(doc, "//figure[.//dyngraphic]")



