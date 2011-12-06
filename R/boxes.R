getRectBoundingBox =
  # For <rect> nodes
function(node)  
{
 vals = as.numeric(xmlAttrs(node)[c("x", "y", "width", "height")])
 matrix(c(vals[1:2], vals[3:4] + vals[1:2]), 2, 2, byrow = TRUE, dimnames = list(c("start", "end"), c("x", "y")))
}

getCircleBoundingBox =
  # For <rect> nodes
function(node)  
{
 vals = as.numeric(xmlAttrs(node)[c("x", "y", "r")])
 matrix(c(vals[1:2] - vals[3], vals[1:2] + vals[3]), 2, 2, byrow = TRUE, dimnames = list(c("start", "end"), c("x", "y")))
}

getUseBoundingBox =
function(node)
{
   getBoundingBox(getUseNode(node))
}

getUseNode =
function(node)
{
   if(xmlName(node) != "use")
     stop("getUseNode requires a <use> element")
   
   href = xmlGetAttr(node, "href")  
   id = gsub("#(.*)", "\\1", href)
   getNodeSet(as(node, "XMLInternalDocument"), sprintf("//*[@id='%s']", id), "x")[[1]] #SVG.xmlns)
}  


getBoundingBox =
  #
  #  This is for a node g that contains a collection of use nodes
  # We get the x and y coordinates for the use nodes and then
  # resolve the glyph links and get the path data for them as a rectangle
  # and determine the extremities of the entire collection of use.
  #
  # Doesn't always return a rectangle/matrix. Can return the shape.
  #
function(gnode, doc = as(gnode, "XMLInternalDocument"))
{
  if(is.character(gnode))
    return(getPathBoundingBox(d = gnode))
  
  if(xmlName(gnode) %in% c( "set", "animateMotion", "animate", "animateColor", "animateTransform"))
    return(NULL)

  if(xmlName(gnode) == "use")
    return(getUseBox(gnode))
  
  if(xmlName(gnode) == "path")
    return(getPathBoundingBox(gnode))

  if(xmlName(gnode) == "rect")
    return(getRectBoundingBox(gnode))

  if(xmlName(gnode) == "circle")
    return(getCircleBoundingBox(gnode))

  if(xmlName(gnode) == "use")
    return(getUseBoundingBox(gnode))

 if(xmlName(gnode) == "symbol")
   return(getBoundingBox(gnode[[1]])) # what if it has multiple children.

  if(xmlName(gnode) == "text") {
    warning("cannot compute bounding box of text reliably")
    return(NULL)
  }

  if(xmlName(gnode) == "g" && !is.null(clip <- xmlGetAttr(gnode, "clip-path"))) {
      
    id = gsub("url\\(#(.*)\\)", "\\1", clip)
    clipPath = getNodeSet(doc, sprintf("//x:clipPath[@id='%s']", id), "x")[[1]] #SVG.xmlns)

    d = xmlGetAttr(clipPath[[1]], "d")

    box = getRect(d)
    return(new("RectangularBoundingBox", box))
  } 

  if(!is.na(xmlGetAttr(gnode, "clip-path", NA)) && xmlSize(gnode) == 1 && names(gnode) == "g")
     gnode = gnode[[1]]
  

  rects = xmlSApply(gnode, function(x) as(getBoundingBox(x), "RectangularBoundingBox"))
  ans = matrix(c(min(rects[1,], na.rm = TRUE), max(rects[2,], na.rm = TRUE),
               min(rects[3,], na.rm = TRUE), max(rects[4,], na.rm = TRUE)), , 2,
               dimnames = list(c("start", "end"), c("x", "y")))
  new("RectangularBoundingBox", ans)
}

getUseBox =
  #
  # given a <use> node, get the box it "covers"
  #
function(node, doc = as(node, "XMLInternalDocument"))
{
   x = xmlGetAttr(node, "x", , as.numeric)
   y = xmlGetAttr(node, "y", , as.numeric)
   r = getGlyphBox(node, doc)

   r[ ,"x"] = x + r[ ,"x"]
   r[ ,"y"] = y + r[ ,"y"]
   r
 }

getGlyphBox =
  #
  # Lookup the glyph and get its bounding rectangle.
  #
function(id, doc)
{
   if(inherits(id, "XMLInternalNode"))
     id = xmlGetAttr(id, "href")
   
   glyphId = gsub("^#", "", id)
   d = getNodeSet(doc, paste("//x:symbol[@id =", sQuote(glyphId), "]/x:path/@d"), "x")[[1]]
   getRect(d)
}  
  

getPathBoundingBox = getShape = 
function(node, d = xmlGetAttr(node, "d"))
{
   if(xmlName(node) == "g" && !is.na(xmlGetAttr(node, "clip-path", NA)) && xmlSize(node) == 1)
      node = node[[1]]

   if(xmlName(node) == "g" && isTextNode(node))
     return(getBoundingBox(node[[1]])) # XXX

   if(is.null(d) || d == "")
     return(NULL)
   
   path = getGeneralPath(d)
   
   if(isCirclePath(path)) 
      makeCirclePath(path)
   else if(isLinePath(path)) 
      makeLinePath(path)   
   else if(isPolylinePath(path)) 
      makePolylinePath(path)
   else if(isPolygonPath(path)) 
      makePolygonPath(path)   
   else
      new("RectangularBoundingBox", getRect(d))
}


setGeneric("isInside",
             function(obj, host) {
                 standardGeneric("isInside")
             })

setMethod("isInside",
           c("XMLInternalNode", "XMLInternalNode"),
          function(obj, host) {
             isInside(getBoundingBox(obj), getBoundingBox(host))
          })

setMethod("isInside",
           c("XMLInternalNode", "RectangularBoundingBox"),
          function(obj, host) {
             isInside(getBoundingBox(obj), host)
          })


setMethod("isInside",
           c("Circle", "RectangularBoundingBox"),
          function(obj, host) {
             obj[1] >= host[1,1] && obj[1] <= host[2,1] &&
               obj[2] >= host[1,2] && obj[2] <= host[2,2]             
          })


foo =
  function(obj, host) {
  #XXX Note that if we do host - obj we get infinite recursion.
  # Need the math operations to work sing groups/math for classes.
  # It is just the matrix actually as we don't need to coerce obj to numeric.
    tmp = as.numeric(host) - as.numeric(obj)
    all(tmp[c(1,3)] <= 0) && all(tmp[c(2,4)] >= 0)
  }  
setMethod("isInside",
           c("RectangularBoundingBox", "RectangularBoundingBox"),
          foo)

setMethod("isInside",
           c("Line", "RectangularBoundingBox"), foo)


setMethod("isInside",
           c("Polyline", "RectangularBoundingBox"),
          function(obj, host) {
             isInside(as(obj, "RectangularBoundingBox"), host)
           })




