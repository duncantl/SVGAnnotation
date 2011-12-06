library(SVGAnnotation)
library(lattice)

doc = svgPlot({
               plot(1:10, pch = c('+', '.'), bg = c("red"), fg = "blue")  
               abline(v = c(3, 7), col = 'blue', lty = 3)
               abline(h = c(3, 7, 9), col = 'lightgray', lty = 2)           
             })
r = getPlotRegionNodes(doc)
length(r)
sapply(r, xmlSize) 

SVGAnnotation:::isAnnotationGroup(r[[2]], r[[1]])


#################


doc = svgPlot({
           par(mfrow = c(1, 2))
           plot(1:26, pch = 0:25, bg = c("red"), fg = "blue")
           plot(1:6, pch = c('A', 'b', '.', '#', '+', '@'))
        })

r = getPlotRegionNodes(doc)
length(r)
sapply(r, xmlSize) # too many in the first one - 43.


SVGAnnotation:::isPlotRegionNew(r[[2]], FALSE)

##########################

doc = svgPlot({
           par(mfrow = c(1,1))
           plot(10, xlim = c(0, 25), ylim = c(0, 25), type = "n")
           sapply(0:25, function(i) points(i, i, pch = i, bg = c("red")))
        })

r = getPlotRegionNodes(doc)
length(r)
sapply(r, xmlSize) # too many in the first one - 43.



par(mfrow = c(1,1))
docs =
  lapply(0:25,
        function(i) {
          svgPlot({
             plot(2, 2, pch = i)
         })})

rr = sapply(docs, getPlotRegionNodes)
sapply(rr, length)

nels = sapply(rr, xmlSize)
table(nels)
(0:25)[which(nels > 1)]



#################################


doc = svgPlot(plot(1:26, pch = 0:25, bg = c("red")))
rr = getPlotRegionNodes(doc)[[1]]

v = groupByPch(xmlChildren(rr), 0:25)

xmlSize(rr)
xmlSApply(rr, xmlSize)
sapply(rr[names(rr) == "g"], xmlSize)



doc = svgPlot(plot(1:10, pch = 8, bg = c("red")))
rr = getPlotRegionNodes(doc)[[1]]
# points don't have the plot-point, yet, but
# in this next call they are there.
pts = getPlotPoints(doc)
xmlSize(rr)
length(rr)

v = groupByPch(pts, rep(8, 10))

xmlSize(rr)
xmlSApply(rr, xmlSize)
sapply(rr[names(rr) == "g"], xmlSize)
names(rr)

xmlAttrs(rr[[1]])
xmlSApply(rr, function(x) "type" %in% xmlAttrs(x))



###############################

pch = c(8,0)
doc = svgPlot(plot(1:10, pch = pch))

pts = getPlotPoints(doc)
 # This code is now
e = numPathElementsPerPCH[rep(as.character(pch), length = length(pts))]
num = rep(seq(along = pts), e)
length(num) = length(pts)

############################

pch = c(8,0, 3)
doc = svgPlot(plot(1:10, pch = pch))

rr = getPlotRegionNodes(doc)[[1]]
pts = getPlotPoints(doc)

xmlSize(rr)

groupByPch(pts, pch)
xmlSize(rr)
xmlSApply(rr, xmlSize)
sapply(rr[names(rr) == "g"], xmlSize)
names(rr)

sapply(rr[names(rr) == "g"], xmlAttrs)
xmlSApply(rr, function(x) "type" %in% names(xmlAttrs(x)))

xmlSApply(rr, xmlGetAttr, "pch")

