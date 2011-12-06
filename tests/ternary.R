if(require(vcd)) {
library(SVGAnnotation)

data(Hitters)
attach(Hitters)
colors <- c("black","red","green","blue","red","black","blue")
pch <- substr(levels(Positions), 1, 1)

svg('ternaryplot.svg')
ternaryplot(
  Hitters[,2:4],
  pch = as.character(Positions),
  col = colors[as.numeric(Positions)],
  main = "Baseball Hitters Data")
dev.off()

library(XML)
library(SVGAnnotation)


doc = xmlParse('ternaryplot.svg')
pts = getNodeSet(doc, "//x:g[@clip-path='url(#clip1)']/x:g", "x")
pts = pts[ sapply(pts, xmlSize) == 1 ]

numberVars = sapply(Hitters, is, "numeric")
Hitters[sapply(Hitters, is, "factor")] = sapply(Hitters[sapply(Hitters, is, "factor")], as.character)

invisible(
 sapply(seq(along = pts),
        function(i) {
          els = Hitters[i, ]
          els[numberVars] = sapply(els[numberVars], formatC, 4)
          newXMLNode("title", paste(names(Hitters), els, sep = " = ", collapse = ", "), parent = pts[[i]])
        }))




# Under #clip
plotRegion = getNodeSet(doc, "//x:g[@clip-path='url(#clip1)']", "x")[[1]]

if(names(plotRegion)[1] == "g")
  titleNode = plotRegion[[1]]

labelNodes = plotRegion[3:5]
tips = c(
         putouts = "A play in which a batter or a baserunner is retired",
         assists = "A fielding and throwing of a baseball in such a way that enables a teammate to put out a runner",
  errors = "A defensive fielding or throwing misplay by a player when a play normally should have resulted in an out or prevented an advance by a base runner")

urls = c(putouts = "http://www.thefreedictionary.com/putout",
         assists = "http://en.wikipedia.org/wiki/Assist_(baseball)",
         error = "http://www.thefreedictionary.com/error"
        )

sapply(seq(along = labelNodes),
       function(i) {
         #addToolTips(labelNodes[[i]], tips[i])
         addLink(labelNodes[[i]], urls[i])
       })


addLink(plotRegion[[1]], "http://bm2.genes.nig.ac.jp/RGM2/R_current/library/vcd/man/Hitters.html")

#
addCSS(doc)

saveXML(doc, docName(doc))

}
