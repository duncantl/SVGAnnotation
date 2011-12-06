library(RGoogleDocs)
google.keys = c(income = "phAwcNAVuyj1jiMAkmq1iMg",
                population = "phAwcNAVuyj0XOoBL_n5tAQ",
                longevity = "phAwcNAVuyj2tPLxKvvnNPA",  # life expectancy
                infantMortality = "phAwcNAVuyj0NpF2PTov2Cw")

gapminder = data =
       lapply(google.keys,
               function(key) {
                 wb = publicWorksheet(key)
                 ws = getWorksheets(wb, NULL)
                 m = sheetAsMatrix(ws[[1]], con = NULL, header = TRUE)
                 rownames(m) = as.character(m[,1])
                 m
               })

# Determine the years and countries common to all data sets.

years = names(data[[1]])
for(i in data[-1])
  years = intersect(years, names(i))

countries = as.character(data[[1]][,1])
for(i in data[-1])
  countries = intersect(countries, as.character(i[,1]))


# Now turn each into value, year, country data frame.
o = lapply(data, function(x) {
                    rownames(x) = as.character(x[,1])
                    tmp = x[countries, years]
                    data.frame(var = unlist(tmp), country = rep(countries, length(years)), year = rep(years, each = length(countries)))
         })

# Combine the data frames into a single data frame.
dd = data.frame(#income = o[[1]][,1], population = o[[2]][,1], longevity = o[[3]][, 1],
                  yr = o[[3]][,"year"], country = o[[3]][, "country"])
dd[names(google.keys)] = lapply(o, function(x) x[,1])

dd$year = as.POSIXct(strptime(as.character(as.integer(as.character(dd$yr))), "%Y"))


save(dd, file = "gapminder.rda")


###############
# 
#

xyplot(longevity ~ income, dd, subset = country == "United States")

G7 = c("United States", "Germany", "Japan", "United Kingdom", "Canada",  "France",  "Italy")
G8 = c(G7, "Russia")

tbls = readHTMLTable("http://en.wikipedia.org/wiki/Member_State_of_the_European_Union")
EU = as.character(tbls[[2]][,3])[1:27]  # 28th is EU-27.



   # European Union.
G20 = c(G8, "Argentina", "Australia", "Brazil", "China", "India", "Indonesia", "Mexico", "Saudi Arabia", "South Africa", "South Korea", "Turkey",
          EU)

# "China", "Australia",  "South Africa",
xyplot(longevity ~ year | country, dd, subset = country %in% G8, auto.key = list(columns = 5))


xyplot(longevity ~ year | country, dd, subset = country %in% G8, auto.key = list(columns = 5),
        panel = function(...) {
           panel.xyplot(...)
           panel.abline(v = as.POSIXct(strptime(as.character(c(1914, 1939)), "%Y")), col = "lightgray", lty = 3)
          })








# Working with the cells feed directly.
if(FALSE) {
sh =  publicWorksheet("phAwcNAVuyj1jiMAkmq1iMg")  
ws = getWorksheets(sh, NULL)  
cells = getURLContent(ws[[1]]@cellsfeed)
doc = xmlParse(cells)

cl = getNodeSet(doc, "/*/x:entry/gs:cell", c("x", "gs" ))

cl10 = getNodeSet(doc, "/*/x:entry/gs:cell[@row = '10']", c("x", "gs" ))
}
