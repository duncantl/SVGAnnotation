##########################
### Code from exLinkPlots.xml

library("SVGAnnotation")

doc = svgPlot({ par(mfrow = c(1, 2))
  plot(mpg ~ wt, mtcars, main="MPG", ylab="", cex = 1.4)
  plot(hp ~ wt, mtcars, main = "Horsepower", ylab="", cex = 1.4)
})

linkPlots(doc)
saveXML(doc, "../mt_link.svg")


doc = svgPlot({par(mfrow = c(1,2))
    plot(Murder ~ UrbanPop, USArrests, main="", cex = 1.4)
    plot(Rape ~ UrbanPop, USArrests, main = "", cex = 1.4)
    }, 
  width = 14, height = 7)


linkPlots(doc)
saveXML(doc, "USArrests_linked.svg")



