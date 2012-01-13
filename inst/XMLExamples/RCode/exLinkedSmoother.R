##########################
### Code from exLinkedSmoother.xml

library("XML")
library("SVGAnnotation")
data("rat.diet", package="fields")
lambdas= 2:floor(0.6 * length(unique(rat.diet$t)))
xrange = range(rat.diet$t)
xinterps = seq(xrange[1], xrange[2], by = 0.5)


doc = svgPlot({
          par(mfrow = c(1, 2))

          plot(con ~ t, data = rat.diet, log = "",
                xlim = xrange, 
                xlab = "Time (days)", ylab = "Median Food Intake", 
                main = "Control group")

          predicted = lapply(lambdas, function(lam) {
                         spl = smooth.spline(rat.diet$t, rat.diet$con, df = lam)
                        lines(predict(spl, xinterps), col="green", lwd = 2)
                        predict(spl, rat.diet$t)
                                     })

          range.y = range(unlist(sapply(predicted, function(pred) {
                       range(rat.diet$con - pred$y)})))
          
          plot(y = 0, x = 0, xlim = xrange,  ylim = range.y, type = "n",
                main = paste("Residual plot"), ylab = "Residuals", 
                xlab = "Time (days)")
          abline(h = 0, col = "lightgray", lwd = 2, lty = 3)
          sapply(predicted, function(p) points(p$x, rat.diet$con - p$y))
       })


plots = getPlotRegionNodes(doc)
lines = getNodeSet(doc, "//x:path[contains(@style, 'rgb(0%,100%,0%)')]", "x")
length(lines) == length(lambdas)


invisible(mapply(function(lam, node) {
                    xmlAttrs(node, append =TRUE) = c(id = lam, visibility = "hidden")
                 }, paste("curve-lambda-", lambdas, sep = ""), lines))

xmlAttrs(lines[[1]], append = TRUE) = c(visibility = "visible")


 numPoints = nrow(rat.diet) 
 points = xmlChildren(plots[[3]])[-1]
 lambdaVal = rep(lambdas, each = numPoints)


  index = matrix(1:length(points), , length(lambdas))
  at = plots[[3]]
  nodes = sapply(seq(along = lambdas),
              function(i) {
              g = newXMLNode("g", attrs = 
                        c(id = paste("residual-group", lambdas[i], sep = "-"),
                          visibility = "hidden"), 
                         parent = at, 
                         namespaceDefinitions = c(xlink = "http://www.w3.org/1999/xlink") )
              removeNodes(points[index[,i]])
              addChildren(g, points[index[,i]])
 
         })
  xmlAttrs(nodes[[1]], TRUE) = c(visibility = "visible")


#jscript = c("../Javascript/linkedSmootherInit.js",
#            "../Javascript/linkedSmootherSet.js")


svgRoot = xmlRoot(doc)
enlargeSVGViewBox(doc, y = 100, svg = svgRoot)
onl = sprintf("init(evt, %d);", max(lambdas) )
jscript = list.files(path = system.file("examples", "Javascript", 
                              package = "SVGAnnotation"), 
                     full.names = TRUE, pattern = "linkedSmoother")
addSlider(doc, onload = onl, svg = svgRoot,
          javascript = jscript, id = "slider-lambda")
saveXML(doc, "linkedSmoother.svg")



