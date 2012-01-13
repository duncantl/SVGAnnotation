##########################
### Code from exJSAnimateElectionMapPaper.xml

library("SVGAnnotation")
library("maps")
data("electionHistory")
doc = svgPlot({
        m <- map('state', fill = TRUE, col = 'grey66')
        title('Presidential Election Results by State 1900-2008')
        text(m$range[1] + 3, m$range[3] + 1, "Start", col = "green")
      })


labels = getAxesLabelNodes(doc)


title = asTextNode(labels$title, 
           'Presidential Election Results by State 1900-2008')


xmlAttrs(title) = c(id = "title")


start = getTextNodes(doc)[[1]]
start = asTextNode(start, "Start")
xmlAttrs(start) = c(id = "Start", onclick = "toggleAnimation(evt)")


addToolTips(start, "Start or pause the animation")


pts = getPlotPoints(doc)
mapply(function(node, id)
            xmlAttrs(node) = c(id = id),
        pts, m$names)


addToolTips(pts, m$names)


polyStateNames = gsub(":.*$", "", m$names)
polyStateNames[ polyStateNames =="district of columbia"] = "d. c."
addECMAScripts(doc, 
               system.file("examples", "JavaScript", 
                           "animateElectionMap.js", 
                            package="SVGAnnotation"),
               stateResultsByYear = electionHistory, 
               yearLabels = names(electionHistory),
               polyIds = m$names,
               polyStateNames = polyStateNames,
               insertJS = TRUE)


convertCSSStylesToSVG(pts)


saveXML(doc, "exJSAnimateElectionMap.svg")



