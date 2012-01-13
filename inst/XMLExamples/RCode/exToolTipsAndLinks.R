##########################
### Code from exToolTipsAndLinks.xml

# Load the required packages 
library("XML")
library("SVGAnnotation")

# Create the basic plot
doc = svgPlot(plot(mpg ~ wt, mtcars, 
                   main = "Motor Trend Car Road Tests", pch=19, 
                   col= "#377EB8"))

 # Annotate title and axes
ax = getAxesLabelNodes(doc)

 # Add link from title to motor trend website
addLink(ax$title, "http://www.motortrend.com", addArea = TRUE) 

 # Add tooltips to the x, y axes
tips = c("Weight in units of lb/1000", "Miles per US gallon")
addToolTips(ax[c("xaxis", "yaxis")], tips) 

addToolTips(doc, apply(mtcars, 1, function(x) 
                       paste(names(mtcars), x, sep = " = ", 
                                   collapse = ", ")))


saveXML(doc, "../mt_tips.svg")


depth.col = gray.colors(100)[cut(quakes$depth, 100, label=FALSE)]
depth.ord = rev(order(quakes$depth))
doc = svgPlot(
 plot(lat ~ long, data = quakes[depth.ord, ],
      pch = 19, col = depth.col[depth.ord],
      xlab = "Longitude", ylab="Latitude",
      main = "Fiji Region Earthquakes") )


ax = getAxesLabelNodes(doc)


addToolTips(ax[c("xaxis", "yaxis")], c("Degrees east of the prime meridean",
            "Degrees south of the equator"), addArea = TRUE)


usgs = "http://earthquake.usgs.gov/eqcenter/recenteqsww/"
region = "Maps/region/S_Pacific.php"
addAxesLinks(ax$title, paste(usgs, region, sep = ""))


addToolTips(doc, 
             apply(quakes[depth.ord, ], 1, 
                    function(x)
                       paste(names(quakes), x, 
                             sep = " = ", collapse = ", ")))


# This is a relative path to the CSS file.
# The default path will be absolute.
#addCSS(doc, css = "../../inst/CSS/RSVGPlot.css")


saveXML(doc, "quakes_tips.svg")



