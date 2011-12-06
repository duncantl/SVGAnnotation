addTypes.boxplot =
function(doc, group = TRUE)
{
  pts = getPlotPoints(doc)
  shapes = sapply(pts, SVGAnnotation:::getShape)

  rect = sapply(shapes, is, "RectangularBoundingBox")
  gboxes = tapply(seq(along = pts), cumsum(rect),
                      function(i)
                        addTypes.boxplotUnit(pts[i], group, shapes[i]))


  
}

addTypes.boxplotUnit =
function(nodes, group = TRUE, shapes = lapply(nodes, getShape))
{
  mapply(function(node, x) {
                    type = if(is(x, "RectangularBoundingBox"))
                                "rectangle"
                           else if(is(x, "Line"))
                                tolower(class(x))
                           else if(is(x, "Circle"))
                                tolower(class(x))
                     type = paste("boxplot", type, sep = "-")
                     setTypeAttr(node, type)
                  }, nodes, shapes)
  
  
  if(group) {
    at = XML:::indexOfNode(nodes[[1]]) - 1L
    parent = xmlParent(nodes[[1]])
    removeNodes(nodes)
    newXMLNode("g", .children = nodes, parent = parent, at = at, attrs = c(type = "boxplot", class = "boxplot"))
  } else
    nodes
}
