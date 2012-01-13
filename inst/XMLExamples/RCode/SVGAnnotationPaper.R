##########################
### Code from exEUseries.xml

library("SVGAnnotation")
data("eu")

eu$Date = as.POSIXct(strptime(eu$Date, "%Y-%m-%d"))

  # Discard currencies that are mostly missing
eu = eu[ , sapply(eu, function(x) sum(is.na(x)))/nrow(eu) < .5]


eu = eu[ !(names(eu) %in% c("KRW", "TRL", "BGN", "ROL")) ]

o = order(sapply(eu[-1], median, na.rm = TRUE), decreasing = TRUE)
eu = eu[c(1, o+1)]


svgPlot({matplot(eu$Date, as.data.frame(lapply(eu[,-1], log)),
                 type = "l", xlab = "Year", xaxt = "n",
                 ylab = "log(exchange rate)",
                 main = "European Exchange Rate")
         startYr = min(eu$Date)
         endYr = max(eu$Date)
         axis.POSIXct(1, at=seq(startYr, endYr, by="year"), 
                     format ="%y")
         abline(h = 1, col = "gray")
         }, 
        "euSeries.svg")


doc = 
  radioShowHide("euSeries.svg", within = TRUE, 
                 labels = currency.names[ match(names(eu)[-1], 
                                          names(currency.names))]
               )



##########################
### Code from exGraphviz.xml

library("XML")
library("SVGAnnotation")


library("Rgraphviz")
library("RJSONIO")
set.seed(123)
V = letters[1:10]
M = 1:4
g1 = randomGraph(V, M, 0.8)


doc = svgPlot(plot(g1, "twopi", 
              attrs = list(node = list(fillcolor = "white"))))


top = xmlRoot(doc)[["g"]][["g"]]
table(names(top))

NULL



layout2pi = agopen(g1, layoutType = "twopi", name = "bob")


#ids = addGraphIds(doc, layout2pi)


cat(toJSON(getEdgeInfo(g1)))

NULL



ids = addGraphIds(doc, layout2pi)
els = getNodeElements(doc)  
sapply(seq(along = els),
          function(i)
            addAttributes(els[[i]], 
             onmouseover = paste("highlightEdges(evt, ", i- 1, 
                                  ", 'chartreuse');"),
             onmouseout = paste("highlightEdges(evt, ", i-1, ");")
                          ))
info = getEdgeInfo(g1) 
names(info) = seq(from = 0, length = length(info))
otherEdges = lapply(info, function(x) setdiff(ids$edgeIds, x))
mapply(addLink, els, ids$nodeIds, MoreArgs = list(silent = TRUE))
jscript = c(system.file("examples", "Javascript", 
                        "highlightEdges.js", 
                         package = "SVGAnnotation"),
            system.file("examples", "Javascript", 
                        "setEdgeStyle.js",
                        package = "SVGAnnotation")
           )
addECMAScripts(doc, jscript, TRUE, edgeTable = info,
                 edgeDiff = otherEdges)
saveXML(doc, "graphviz.svg")



##########################
### Code from exHexbinToolTips.xml

library("SVGAnnotation")
data("traffic")
Occupancy = unlist(traffic[ c("Occ1", "Occ2", "Occ3")])
Flow = unlist(traffic[c("Flow1", "Flow2", "Flow3")])


library("hexbin")
hbin = hexbin(Occupancy, Flow)
doc = svgPlot(
        plot(hbin, 
             main = "Loop Detector #313111 on I-80E Sacramento"))


ptz = getPlotPoints(doc)
length(ptz)

NULL

length(hbin@count)

NULL

tips = paste("Count: ", hbin@count)
addToolTips(ptz, tips, addArea = TRUE)


saveXML(doc, "hexbin.svg")



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




##########################
### Code from exLinkPlots.xml

library("SVGAnnotation")

doc = svgPlot({ par(mfrow = c(1, 2))
  plot(mpg ~ wt, mtcars, main="MPG", ylab="", cex = 1.4)
  plot(hp ~ wt, mtcars, main = "Horsepower", ylab="", cex = 1.4)
})

linkPlots(doc)
saveXML(doc, "../mt_link.svg")


doc = svgPlot({par(mfrow = c(1,2))
    plot(Murder ~ UrbanPop, USArrests, main="", cex = 1.4)
    plot(Rape ~ UrbanPop, USArrests, main = "", cex = 1.4)
    }, 
  width = 14, height = 7)


linkPlots(doc)
saveXML(doc, "USArrests_linked.svg")



##########################
### Code from exStateElectionTable.xml

library(SVGAnnotation)
data(election2008)


capitalize =
function(x)
{
  e = strsplit(x, "[[:space:]]")[[1]]
  paste(sapply(e, function(o) paste(toupper(substring(o, 1, 1)), 
        substring(o,2), sep = "")), collapse = " ")
}


makeStateTable =
function(info, name, asString = TRUE)
{
  totals = colSums(info[,1:2])
  total = sum(totals)

  obama.won = info[,1] > info[,2]

  div = newXMLNode("div")
  newXMLNode("h2", capitalize(name), parent = div)
  diva = newXMLNode("div", parent = div, attrs = c(id = "diva"))  
  p = newXMLNode("p", "Winner: ",  
      newXMLNode("b", if(totals[1] > totals[2]) "Obama" else "McCain",
      attrs = c(class = if(totals[1] > totals[2]) "obama" else "mccain")), 
      parent = diva)  
  p = newXMLNode("p", "Number of votes:", parent = diva)
  
  tb = newXMLNode("table", parent = p)
  tr = newXMLNode("tr", newXMLNode("th", "Total"),
                   newXMLNode("td", total),
                   parent = tb, attrs = c(align = "right"))  
  tr = newXMLNode("tr", newXMLNode("th", "Obama"),
                  newXMLNode("td", totals[1]),
                  newXMLNode("td", sprintf("  (%.0f%%)", 100*totals[1]/total)),
                  parent = tb, attrs = c(align = "right"))
 tr = newXMLNode("tr", newXMLNode("th", "McCain"),
                 newXMLNode("td", totals[2]),
                 newXMLNode("td", sprintf("  (%.0f%%)", 100*totals[2]/total)),
                 parent = tb, attrs = c(align = "right"))

  divb = newXMLNode("div", parent = div, attrs = c(id = "divb"))    
  p = newXMLNode("p", "Number of counties:", parent = divb)
  tb = newXMLNode("table", parent = p)
  tr = newXMLNode("tr", newXMLNode("th", "Total"),
                  newXMLNode("td", nrow(info)),
                  parent = tb, attrs = c(align = "right"))  
  tr = newXMLNode("tr", newXMLNode("th", "Obama"),
                  newXMLNode("td", sum(obama.won)),
                  parent = tb, attrs = c(align = "right"))
  tr = newXMLNode("tr", newXMLNode("th", "McCain"),
                  newXMLNode("td", sum(!obama.won)),
                  parent = tb, attrs = c(align = "right"))

  if(asString)
     saveXML(div)
  else
     div
}


makeCountiesTable =
function(info, name, asString = TRUE)
{
  obama.won = info[,1] > info[,2]
  
  tb = newXMLNode("table")
  newXMLNode("tr", newXMLNode("th"), newXMLNode("th", "County"), 
             newXMLNode("th", "Obama"), newXMLNode("th", "McCain"), 
             parent = tb)

  sapply(seq(length = nrow(info)),
          function(i) {
            x = info[i,]
            tr = newXMLNode("tr", 
                 attrs = c(align = "left", 
                   class = if(obama.won[i]) "obama" else "mccain"), parent = tb)
            newXMLNode("td", i, parent = tr)            
            newXMLNode("td", info[i, 3], parent = tr)
            newXMLNode("td", info[i, 1], attrs = c(align = "right"), 
                       parent = tr)
            newXMLNode("td", info[i, 2], attrs = c(align = "right"), 
                       parent = tr)                        
          })

  if(asString)
    saveXML(tb)
  else
    tb
}


library(maps)
m = map('state', fill = TRUE, col = "red")
stateNames = gsub(":.*", "", m$names)
polyNames = gsub("'", "", m$names)

obama.won = sapply(states, function(x) diff(colSums(x[, 1:2]))) < 0
names(obama.won) = gsub("-", " ", names(states))


setdiff(stateNames, names(obama.won))
setdiff(names(obama.won), stateNames)

doc = svgPlot(map('state', fill = TRUE, 
              col = c("red", "blue")[obama.won[stateNames] + 1]))
p = getPlotPoints(doc)
mapply(function(node, id) xmlAttrs(node) = c(id = id),
        p, polyNames)
addToolTips(p, stateNames, addArea = 2)
#mapply(function(node, id) newXMLNode("title", id, parent = node),  p, stateNames)
saveXML(doc, "stateMap.svg")


 library(XML)
 names(states) = gsub("-", " ", names(states)) 
 stateTables = mapply(makeStateTable, states, names(states))
 stateTables["national"] = makeStateTable(allStates, "National")
 countyTables = mapply(makeCountiesTable, states, names(states))
 
 library(RJSONIO)
 cat("var stateSummaryTables = ", toJSON(stateTables), ";",
     "var countyTables = ",  toJSON(countyTables), ";",
     "var polyIds = ", toJSON(polyNames), ";",
     "var polyStates = ", toJSON(stateNames), ";",
     file = "stateHTMLTables.js")  



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



##########################
### Code from exWorldAnimation.xml

library("RColorBrewer")
library("SVGAnnotation")
library("XML")

fixNAs = function(x) {
  na = is.na(x)
  if(!any(na))
    return(x)

  NApos = which(na)
  notNA = which(!na)
  
  if (any(NApos < min(notNA))) {
    NApos = NApos[ NApos > min(notNA)]
  }

  if (length(NApos))
    x[ NApos ] =  x[sapply(NApos, function(i) notNA[max(which(notNA < i))] )]

  return(x)
}

firstNAs = function(x) {
 na = is.na(x)
 if (!any(na))
   return(x)

 NApos = which(na)
 notNA = which(!na)

 if (length(NApos)) {
    x[NApos] = x[notNA[1]] 
 }

 return(x)
}

load(system.file("examples", "GapMinder", "gapminder.rda", package = "SVGAnnotation"))


ctry = c("Argentina", "Australia", "Austria", "Bangladesh", "Belgium","Brazil", 
        "Bulgaria", "Canada",  "Chile", "China", "Colombia", "Costa Rica", "Cuba", "Cyprus",
        "Denmark", "Fiji", "Finland","France", "Germany", "Ghana", "Greece", "Guatemala",
        "Hungary", "India", "Indonesia", "Ireland", "Italy", "Jamaica", "Japan", 
        "Netherlands", "Norway", "Peru", "Philippines", 
        "Portugal", "United Kingdom", "United States")


yrA = cut(dd$year, "year")
yr4 = gsub("-.*", "", yrA)
yrA = trunc(as.integer(yr4)/10) * 10
yr = yrA

cont = c("AM", "EA", "EU", "SA", "EU", "AM", 
         "EU", "AM", "AM", "EA", "AM", "AM", "AM", "EU", 
         "EU", "EA", "EU", "EU", "EU", "SS", "EU", "AM", 
         "EU", "SA", "EA", "EU", "EU", "AM", "EA", 
         "EU", "EU", "AM", "EA", 
         "EU", "EU", "AM")


keep = (dd$country %in% ctry) & (yr >= 1890)
yr = yr[keep]
dd = dd[keep, ]

vars = c("longevity", "income", "population")
gapM = list()


for(i in vars) {
  x = dd[[i]]
  for (j in ctry) {
     x[dd$country == j] = fixNAs(x[dd$country == j])
  }
  y = tapply(x, data.frame(yr, dd$country), max, na.rm = TRUE)

  w = y[, levels(dd$country) %in% ctry]
  w = as.vector(t(w[-1, ]))
  w[w == -Inf] = NA
  gapM[[i]] = w 
}
  
gapM = as.data.frame(gapM) 

nc  = length(unique(dd$country))
ny  = length(unique(yr[yr>1890]))

decade = rep(unique(yr[yr>1890]), rep(nc, ny))
gapM$yr = decade   
country = rep(unique(dd$country), ny)
gapM$country = country


head(gapM)

NULL

tail(gapM)

NULL



for (i in vars) {
  for (j in ctry) {
    gapM[gapM$country == j, i] = firstNAs(gapM[gapM$country == j, i])
  }
}


rad = 1 + 10 * sqrt(gapM$population)/max(sqrt(gapM$population))
disappear = is.na(gapM$longevity) | is.na(gapM$income) | 
            is.na(gapM$population)
rad[disappear] = 0.00001 
radL = lapply(ctry, function(i) rad[gapM$country == i])
names(radL) = ctry


colI = c("#E41A1C80", "#377EB880", "#4DAF4A80", "#984EA380", "#FF7F0080", "#FFFF3380")
colB = c("#E41A1CFF", "#377EB8FF", "#4DAF4AFF", "#984EA3FF", "#FF7F00FF", "#FFFF33FF")
names(colB) = c("EA", "AM", "ME", "SA", "SS", "EU")
names(colI) = names(colB)
colsB = colB[cont]
colsI = colI[cont]
longCont = c("East Asia", "America", "Middle East", "South Asia", 
              "Sub-Sahara Africa", "Europe & Central Asia")


doc = svgPlot( {
  plot(longevity ~ income, 
          subset(gapM, yr == 1900 & country %in% ctry), 
    pch = 21, col = colsB, bg = colsI,
    xlab = "Income", ylab = "Life Expectancy", 
    axes = FALSE, 
    xlim = c(-400, 50000), ylim = c(20, 85) )
    box()
    y.at = seq(20, 85, by = 5)
    x.at = c(200, 400, 1000, 2000, 4000, 10000, 20000, 40000) 
    axis(1, at = x.at, labels = formatC(x.at, big.mark = ",", 
                                         format = "d"))
    axis(2, at = y.at, labels = formatC(y.at) )
    abline(h = y.at, col="gray",  lty = 3)
    abline(v = x.at, col="gray",  lty = 3)
    legend(35000, 40, longCont, col = colB, fill = colB, 
           bty = "n", cex = 0.75 )
   })


addToolTips(doc,
   as.character(gapM$country[gapM$yr == 1900 & gapM$country %in% ctry]))


animate(doc, 
        gapM[gapM$country %in% ctry, c("income", "longevity")], 
        gapM$yr[gapM$country %in% ctry], 
        dropFirst = TRUE,
        labels = seq(1900, length = 11, by = 10),
        begin = 0, interval = 3,  radii = radL[ctry])


saveXML(doc, "gapM.svg")



