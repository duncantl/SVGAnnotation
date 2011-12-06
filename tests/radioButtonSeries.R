if(FALSE) {
library(SVGAnnotation)

x = rnorm(100)
y = rnorm(200, 1, .5)

svg('densities.svg')
  plot(density(x), ylim = c(0, 1))
  lines(density(y), col = "red")
dev.off()


radioShowHide("densities.svg")
}
