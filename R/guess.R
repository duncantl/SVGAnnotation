####
# Guessing the type of content in an SVG node, i.e. its simpler, higher-level type.

getSVGShape =
function(node)
{
  if(xmlName(node) == "g" && xmlSize(node) == 1 && xmlName(node[[1]]) == "g")
     return(getSVGShape(node[[1]]))

  if(xmlName(node) == "g" && all(xmlSApply(node, xmlName) == "use"))
      return("text")

  if(xmlName(node) == "g" && xmlSize(node) == 1 && xmlName(node[[1]]) == "path")
    return(getSVGShape(node[[1]]))

  if(xmlName(node) == "path") {
    p = xmlGetAttr(node, "d")
    guessPathShape(p, node)
  } else
    NA
}

guessPathShape =
function(p, node = NULL)
{
  k = class(getBoundingBox(node))
  if(k == "RectangularBoundingBox")
    k = "rectangle"

  k
}


guessSVGShape =
function(doc, addShapes = TRUE, recursive = FALSE, ...)
  UseMethod("guessSVGShape")

guessSVGShape.XMLInternalNode =
function(doc, addShapes = TRUE, recursive = FALSE, ...)
{
   type = getSVGShape(doc)
   if(addShapes)
     setShape(doc, type)
   as.character(type)
}


guessSVGShape.XMLInternalDocument =
function(doc, addShapes = TRUE, recursive = FALSE, ...)
{
   type = getSVGShape(doc)
   sapply(getPlotPoints(doc),  guessSVGShape, addShapes, recursive)   
}

guessSVGShape.XMLInternalNodeList = guessSVGShape.XMLNodeSet = guessSVGShape.list = guessSVGShape.AxesLabelNodes =
function(doc, addShapes = TRUE, recursive = FALSE, ...)  
{
  mapply(guessSVGShape, doc, MoreArgs = list(addShapes, recursive))
}


setShape =
function(node, type)
{
 if(!is.null(node) && !is.na(type)) {
     xmlAttrs(node) = c(shape = type)
  }
 node
}
