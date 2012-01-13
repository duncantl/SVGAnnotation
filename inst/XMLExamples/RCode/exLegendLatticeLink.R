##########################
### Code from exLegendLatticeLink.xml

library("XML")
library("SVGAnnotation")


library("lattice")
mtcars$cyl = factor(mtcars$cyl,
                    labels = paste(c("four", "six", "eight"), 
                                   "cylinder"))
mtcars$am = factor(mtcars$am, labels = c("automatic", "manual"))
gearGroups = sort(unique(mtcars$gear))
gearLabels = paste(gearGroups, "gears", sep = " ")
colors = 
 trellis.par.get("superpose.symbol")$col[seq(along = 
                                             levels(mtcars$cyl))]
topArgs = list(fun = draw.key, 
               args = list(key = list(text = list(gearLabels), 
                                      columns = 3)))
botArgs = 
  list(fun = draw.key,
       args = list(key = list(text = list(levels(mtcars$cyl)),
                              points = list(pch = 21,
                                            col = colors),
                              columns = 3)))
doc = 
 svgPlot(xyplot(mpg ~ disp| am, groups = cyl, data = mtcars,
                legend = list(top = topArgs, bottom = botArgs)))


panels = getPlotRegionNodes(doc)
points = unlist(lapply(panels, xmlChildren), recursive = FALSE)


ids = by(mtcars, list(mtcars$gear, mtcars$am), 
         function(x) paste(as.numeric(x$am), x$gear, 1:nrow(x), 
                             sep = "-"))
uids = unlist(ids) 
mapply(function(node, id)
        addAttributes(node, id = id), points, uids)


counts = table(mtcars$am, mtcars$gear)
counts

NULL



nodes = getLatticeLegendNodes(doc, panels, 1)
sapply(seq(along = 1:length(gearGroups)), 
       function(i) {
        cts = paste("[", paste(counts[,i], collapse = ", "), "]", 
                    sep = "")
        addAttributes(nodes[[i]], 
          onmouseover = paste("highlight(", gearGroups[i], ",", 
                              cts, ", true)"),
          onmouseout = paste("highlight(", gearGroups[i], ",", 
                              cts, ", false)")
        )
       }
      )


#jscript=c("../Javascript/multiLegendHighlight.js",
#          "../Javascript/multiLegendHighlightPoint.js")


jscript = list.files(system.file("examples", "Javascript", 
                       package="SVGAnnotation"), 
                     full.names = TRUE, pattern = "multiLegend")
addECMAScripts(doc, jscript)
saveXML(doc, "mt_lattice.svg")


addECMAScripts(doc, I("highlight(4, [4, 8], false);"))
saveXML(doc, "mt_lattice_gears.svg")

FALSE
FALSE
FALSE


