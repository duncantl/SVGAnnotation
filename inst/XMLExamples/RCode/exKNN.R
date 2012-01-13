##########################
### Code from exKNN.xml

library("SVGAnnotation")
library("XML")


doc = svgPlot(plot(mpg ~ wt, mtcars, 
                   main = "Motor Trend Car Road Tests", 
                   pch=19, col= c("green", "blue")[(am+1)]))


ptz = getPlotPoints(doc, simplify = FALSE)[[1]]
sapply(seq(along = ptz), function(i) 
          addAttributes(ptz[[i]], id = i - 1))

 
sapply(seq(along = ptz), 
       function(i) { 
        addAttributes(ptz[[i]], 
                      onmouseover = 
                        "showNeighbors(evt, k, neighbors)", 
                      onmouseout = "hideLines(evt)") 
       }) 


DD = as.matrix(dist(mtcars[, c("mpg", "wt")]))
D = t(apply(DD, 1, order)) - 1


#jscript=c("../Javascript/knnMain.js", "../Javascript/knnAddLines.js",
#          "../Javascript/knnHideLines.js")


dimnames(D) = list(NULL, NULL)
jscript = list.files(path = system.file("examples", "Javascript", 
                                        package = "SVGAnnotation"), 
                     full.names = TRUE, pattern="knn")
addECMAScripts(doc, jscript, TRUE, neighbors = D) 


saveXML(doc, "mt_knn.svg")



