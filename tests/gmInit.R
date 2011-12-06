#
# 4 points
#  1 going along a 45 degree line
#  one going 
#

p1 = matrix(c( 2, 2, 2,
               3, 3, 3,
               4, 4, 4,
               5, 5, 5, 
               6, 6, 6), 5, , byrow = TRUE)

p2 = matrix(c( 12, 2, 1,
               10, 3, 1.2,
                8, 4, 1.4,
                6, 5, 1.5, 
                4, 6, 3), 5, , byrow = TRUE)


p3 = cbind(seq(4, 12, length = 5),
           rep(6, 5),
           c(1.5, 3, 5, 3, 1.5))

D = data.frame(rbind(p1, p2, p3))

names(D) = c("x", "y", "radius")
D$time = rep(1:5, 3)
D$id = rep(1:3, each = 5)
D = D[order(D$time),]


if(FALSE) {
plot(0, xlim = range(D$x), ylim = range(D$y))
by(D, D$id, function(x){
             col = c("red", "green", "blue")[x[,"id"]]
             points(x[,1], x[,2], col = col)
             text(x[,1] + .1, x[,2] + .1, 1:nrow(x), col = col)             
             lines(x[,1:2], col = col[1])
           })
}
