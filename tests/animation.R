# Let's create fake data.

library(SVGAnnotation)

years = 1900:2008
n = length(years)
groupIds = LETTERS[1:7]
numGroups = length(groupIds)
alpha = seq(0, by = 10, length = numGroups)
beta = sample(seq(-1, 1, length = numGroups))
sigma.x = seq(.5, 5, length = numGroups)
sigma.y = sample(sigma.x)
data = do.call("rbind",
         lapply(seq(along = alpha),
                   function(i) {
                      x = alpha[i] + beta[i]*(1:n) + rnorm(n, sigma.x[i])
                      y = alpha[i] + beta[i]*x + rnorm(n, sigma.y[i])
                      data.frame(x = x, y = y)
                   }))

data$year = rep(years, numGroups)
data$groupIds = factor(rep(groupIds, each = n))

radii = sapply(rnorm(numGroups, diff(range(data$x))/100), max, .1)

########################################

filename = "anim.xml"

i = seq(1, nrow(data), by = n)

doc = svgPlot({
       with(data, plot(y ~ x, xlim = range(x), ylim = range(y), type = "n", xlab = "X", ylab = "Y"))
       abline(v = pretty(data$x), col = "lightgrey", lty = 3)
       abline(h = pretty(data$y), col = "lightgrey", lty = 3)
       symbols(data$x[i], data$y[i], circles = radii, inches = FALSE,
                bg = rainbow(length(levels(data$groupIds[i])))[data$groupIds[i]], add = TRUE)
})


animate(doc, data, data$groupIds, dur = n/.5,
          colors = character(), points = getPlotPoints(doc)[1:numGroups], doc = doc)

saveXML(doc, filename)



