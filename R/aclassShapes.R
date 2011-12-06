setClass("SVGPath", contains = "matrix")

setAs("character", "SVGPath",
       function(from)
         getPath(from))

setAs("XMLInternalNode", "SVGPath",
       function(from) {
         d = xmlGetAttr(from, "d", NA)
         if(is.na(d))
           return(new("SVGPath", matrix(, 0, 0)))
                  
         getPath(d)
       })


setClass("Shape", contains = "VIRTUAL")
setClass("Circle", contains = c("numeric", "Shape"))
setClass("Polygon", contains = c("list", "Shape"))

setClass("Polyline", contains = c("list", "Shape"))

setClass("Line", contains = c("numeric", "Shape"))
setClass("VerticalLine", contains = 'Line')
setClass("HorizontalLine", contains = 'Line')

setClass("RectangularBoundingBox", contains = c("matrix", "Shape"))

setAs("RectangularBoundingBox", "Circle",
       function(from) {
          new("Circle", structure(c(colMeans(from), min(apply(from, 2, diff))/2), names = c("x", "y", "r")))
       })

setAs("Line", "RectangularBoundingBox", 
       function(from) {
          m = matrix(from, 2, 2, byrow = TRUE, dimnames = list(c("start", "end"), c("x", "y")))
          new("RectangularBoundingBox", m)
       })

setAs("Circle", "RectangularBoundingBox", 
       function(from) {
          x = c(from[1:2] - from[3], from[1:2] + from[3])
          m = matrix(x, 2, 2, byrow = TRUE, dimnames = list(c("start", "end"), c("x", "y")))
          new("RectangularBoundingBox", m)
       })


setAs("Polyline", "RectangularBoundingBox", 
       function(from) {
          pts = do.call("rbind", from)
          rr = apply(pts, 2, range)
          dimnames(rr) = list(c("start", "end"), c("x", "y"))
          new("RectangularBoundingBox", rr)
       })
