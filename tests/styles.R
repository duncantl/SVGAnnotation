library(SVGAnnotation)
doc = xmlParse("multipanelLatticeLegend.svg")
z = compressStyles(doc, add = TRUE)

 # Just check what it looks like
getNodeSet(z, "//style")
unlist(getNodeSet(z, "//@class"))

saveXML(z, "bob.svg")

s = file.info(c("bob.svg", "multipanelLatticeLegend.svg"))$size
s[1]/s[2] # 68%


#####################

if(file.exists("linkedSmoother.svg")) {
doc = xmlParse("linkedSmoother.svg")
z = compressStyles(doc, add = TRUE)

saveXML(z, "bob.svg")
s = file.info(c("bob.svg", "linkedSmoother.svg"))$size
s[1]/s[2] # 64%
}
