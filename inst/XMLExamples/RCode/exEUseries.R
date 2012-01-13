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



