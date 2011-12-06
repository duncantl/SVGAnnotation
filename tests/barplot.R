library(SVGAnnotation)
load("browserUsage.rda")

doc = svgPlot(barplot(structure(as.numeric(webBrowsers[nrow(webBrowsers), -c(1,11) ]), names = names(webBrowsers)[-c(1,11)]),
                      horiz = TRUE),
              width = 10,  height = 13)

if(FALSE) {
    # this is appropriate for some (earlier) versions of cairo
    #  < 1.8.10, at least.
 g = getNodeSet(doc, "//x:g[@id and starts-with(@id, 'surface')]", "x")[[1]]
 txt = xmlChildren(g)[xmlSApply(g, SVGAnnotation:::isTextNode)]
} else {
  
  txt = getTextNodes(doc)    # , "//x:g[count(./x:use) = count(./x:*)]", "x")
}

vtxtNodes = txt[sapply(txt, SVGAnnotation:::isVerticalText)]
    # now which go with which. Not all labels will be displayed unless we have a big (tall) enough
    # canvas.  We do here, by design. 
labels = names(webBrowsers)[-c(1, 11)]
vtxtNodes  = vtxtNodes[seq(along = labels)]

addToolTips(vtxtNodes, text = labels)


g = getNodeSet(doc, "//x:g//x:path[not(parent::x:symbol)]", "x")

shapes = sapply(g, SVGAnnotation:::guessSVGShape)
# These are the bars in the bar plot.
rect = g[shapes == "rectangle"]



saveXML(doc, "browserBarplot.svg")


##########################

# http://www.math.grin.edu/~rebelsky/Courses/MAT115/2008S/R/stacked-bar-graphs.html
EnvironmentSpending = data.frame(
  Liberal = c(.819, .174, .007),
  Moderate = c(.619, .314, .067),
  Conservative = c(.479, .385, .136)
)

st = svgPlot(barplot(as.matrix(EnvironmentSpending)))

g = xmlRoot(st)[["g"]][["g"]]

#addToolTips(xmlChildren(g), 

side = svgPlot(barplot(as.matrix(EnvironmentSpending), beside = TRUE))

g = xmlRoot(st)[["g"]][["g"]]





###################################################

          
va = svgPlot(mp <- barplot(t(VADeaths)[, 5:1], beside = TRUE,
        col = c("lightblue", "mistyrose",
                "lightcyan", "lavender"),
        legend = colnames(VADeaths), 
        main = "Death Rates in Virginia", font.main = 4,
        cex.names = 1.5))

g = xmlRoot(va)[["g"]][["g"]]
