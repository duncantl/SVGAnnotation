##########################
### Code from exLatticeChoiceHTML.xml

library("XML")
library("SVGAnnotation")
library("lattice")
library("RJSONIO")


mtcars$cyl = factor(mtcars$cyl, 
                    labels = c("four cyl", "six cyl", "eight cyl"))
mtcars$am = factor(mtcars$am, labels = c("automatic", "manual"))
doc = svgPlot(xyplot(mpg ~ hp | am, groups =cyl, data = mtcars))


panels = getPlotRegionNodes(doc)
points = unlist(lapply(panels, xmlChildren), recursive = FALSE)
ids = by(mtcars, list(mtcars$gear, mtcars$am), 
         function(x) paste(as.integer(x$am), x$gear - 2, 1:nrow(x), 
                           sep = "-")
        )
uids = unlist(ids) 
mapply(function(node, id) addAttributes(node, id = id),
       points, uids)
saveXML(doc, "mt_lattice_Choice.svg")


counts = table(mtcars$am, mtcars$gear)
rownames(counts) = NULL
dfCounts = list()
for (i in 1:ncol(counts)) 
{
  dfCounts[[i]] = counts[, i]
}


#htmlSkel = htmlParse("mt_lattice_Choice_Skel.html")
#jscript = c("../Javascript/latticeChoiceHighlight.js",
#            "../Javascript/latticeChoiceHighlightPoint.js",
#            "../Javascript/latticeChoiceShowChoice.js")


htmlSkel = htmlParse(system.file("examples", "HTML",
              "mt_lattice_Choice_Skel.html", package="SVGAnnotation"))
jscript = list.files(path = system.file("examples", "Javascript", 
                              package = "SVGAnnotation"), 
                     full.names = TRUE, pattern="latticeChoice")
            
addECMAScripts(htmlSkel, scripts = jscript, 
                insertJS = TRUE, .jsvars = list(pointCounts = dfCounts))

saveXML(htmlSkel, "mt_lattice_Choice.html")


rm(mtcars)



