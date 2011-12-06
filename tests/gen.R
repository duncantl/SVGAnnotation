# These are single letter xlab and ylab & are at the end of the document.

svg("labels.svg")
plot(rnorm(101), main = "The title", xlab = "x", ylab = "y")
dev.off()

svg("scatterplot.svg")
plot(rnorm(101), main = "The title")
dev.off()

svg("density_histogram.svg")
x = rnorm(1000)
hist(x, prob = TRUE)
curve(dnorm(x), min(x), max(x), add = TRUE, col = "red", lty = 3)
dev.off()

svg("hist.svg")
x = rnorm(1000)
hist(x, prob = TRUE)
dev.off()

svg("hist_labels.svg")
x = rnorm(1000)
hist(x, prob = TRUE, xlab = "X", ylab = "Probability", main = "Normal")
dev.off()


svg("hist_plotmath.svg")
x = rnorm(1000)
hist(x, prob = TRUE, xlab = "X", ylab = "Probability", main = expression(N(mu, sigma)))
dev.off()


svg("pairs.svg")
pairs(mtcars)
dev.off()

library(lattice)
svg("barchart.svg")
barchart(table(sample(c("A", "B", "C"), 200, replace = TRUE)))
dev.off()

svg("vertical_barchart.svg")
barchart(table(sample(c("A", "B", "C"), 200, replace = TRUE)), horiz = FALSE)
dev.off()

svg("mfrow.svg")
par(mfrow = c(3, 2))
for(i in 1:6) 
   plot(rnorm(i*20), rnorm(i*20))
dev.off()


svg("layout.svg")
m = matrix(1, 10, 10)
m[2:4, 2:5] = 2
layout(m)
plot(1:100)
plot(1:10)
dev.off()


svg("layout_density.svg")
layout(rbind(c(0, 4, 4, 0),
             c(0, 2, 0, 0),
             c(0, 1, 3, 0),
             c(0, 0, 0, 0)),
         height = c(lcm(2), lcm(3), 1, lcm(2)),
         width = c(lcm(2), 1, lcm(4), lcm(1)))
   x = rnorm(50)
   y = 10*x + 1 + rnorm(length(x))
  par(mar = rep(0, 4))
  plot(x, y)
  plot(density(x), axes = FALSE, main = "")

  d = density(y)
  plot(d$y, d$x, type = "l", axes = FALSE, main = "")
  plot(0, xlim = c(0, 1), ylim = c(0, 1), type = "n", axes = FALSE)
  text(.5, .5, "X versus Y with densities", cex = 2, font = 2) # bold font.
dev.off()

svg("boxplot.svg")
boxplot(list(rnorm(100), rnorm(200, 4, 10)), xlab = "Normals", main = "Different normals")
dev.off()

svg("../tests/plotmath.svg")
plot(1:10, main = expression(sum(x[i], i, n)))
dev.off()



svg("../tests/xyplot.svg")
xyplot(y ~ x | f, data.frame(x = rnorm(100), y = rnorm(100), f = gl(5, 20)), main = "Conditional X-Y plot")
dev.off()
