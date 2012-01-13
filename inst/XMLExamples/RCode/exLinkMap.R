##########################
### Code from exLinkMap.xml

library("XML")
library("SVGAnnotation")
library("maps")
data("election2008")


stateO = sapply(states, function(x) sum(x$Obama))
stateM = sapply(states, function(x) sum(x$McCain))
winner = 1 + (stateO > stateM)


regions = gsub("-", " ", names(winner)) 
stateInd = match.map("state", regions)
polyWinners = winner[stateInd]
stateColors =  c("#E41A1C", "#377EB8")[polyWinners]
doc = svgPlot({
                map('state', fill = TRUE, col = stateColors)
                title("Election 2008")
              }
             )


polygonPaths = getPlotPoints(doc)
length(polygonPaths)
NULL

length(stateInd)
NULL

urls = paste("http://elections.nytimes.com/2008/results/states/", 
             names(winner)[stateInd],".html", sep="")


addLink(polygonPaths, urls, css = character())


saveXML(doc, "stateLinkMap.svg")


polygonAlt = getNodeSet(doc, "/svg:svg/svg:g/svg:g//svg:path", "svg")

length(polygonAlt)

NULL




