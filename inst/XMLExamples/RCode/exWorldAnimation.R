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



