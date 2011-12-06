x = c(1, 3, 5, 2, 4, 6, 3, 4, 5)
y = c(1, 1, 1, 3, 3, 3, 5, 5, 5)
tt = rep(c("A", "B", "C"), each = 3)

d = data.frame(x = x, y = y, time = tt)
library(SVGAnnotation)

doc = svgPlot(with(subset(d, time == "A"), plot(x, y)))



