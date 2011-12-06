library(SVGAnnotation)
library(XML)

if(!file.exists("simple_link.xml")) {
   svg("simple_link.xml")
   par(mfrow = c(2, 2))
   replicate(4, plot(1:10))
   dev.off()
}

doc = xmlParse("simple_link.xml")
linkPlots(doc)
saveXML(doc, "links.xml")



#####################


svg("mtcars_link.svg", 10, 10)
par(mfrow = c(1, 3), cex.lab = 2, cex.main = 2, cex = 1.4)
plot(mpg ~ cyl, mtcars)
plot(hp ~ wt, mtcars)
plot(carb ~ hp, mtcars)   
dev.off()

doc = xmlParse("mtcars_link.svg")
linkPlots(doc)
saveXML(doc, "mtcars_link.svg")



svg("pairs_link.svg", 14, 10)
pairs(mtcars[,1:3], cex = 2)
dev.off()

doc = xmlParse("pairs_link.svg")
linkPlots(doc)
saveXML(doc, "pairs_link.svg")


