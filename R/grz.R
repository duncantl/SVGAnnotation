

################################


getPlotRegion =
  #
  # returns the rectangle giving the coordinates of the plot.
  # Takes a plot region node which has a clip-path attribute.
  #
function(node, doc = as(node, 'XMLInternalDocument'))
{
   clip = xmlGetAttr(node, "clip-path")
   id = gsub("url\\(#(.*)\\)", "\\1", clip)
   clipPath = getNodeSet(doc, paste("//x:clipPath[@id =", sQuote(id), "]"), "x")[[1]]
   getRect(xmlGetAttr(clipPath[["path"]], "d"))
}

getPlotRects =
function(doc)
  UseMethod("getPlotRects")


getPlotRects.XMLInternalDocument =
function(doc)
{
   reg = getPlotRegionNodes(doc)
   lapply(reg, getPlotRegion, doc)
}


#################################

getPlotPoints =
function(doc, simplify = TRUE, addTypes = TRUE, ...)
  UseMethod("getPlotPoints")

getPlotPoints.XMLInternalDocument =
  # Simple scatter plot() only
  #
  #XXX Need to be more general to get all of them.
function(doc, simplify = TRUE, addTypes = TRUE, ...)  
{
#  getNodeSet(doc, "//x:g[starts-with(@clip-path, 'url(#clip')]/x:path", "x")
  ans = lapply(getPlotRegionNodes(doc), getPlotPoints)

  ans = ans[ sapply(ans, length) >  0]
  
  if(simplify && length(ans) == 1)
    ans[[1]]
  else
    ans
}

getPlotPoints.XMLInternalNode =
  # Simple scatter plot() only
  #XXX Need to be more general to get all of them.
function(doc, simplify = TRUE, addTypes = TRUE, ...)  
{
  tmp = getNodeSet(doc, "./*[@type = 'plot-point']")
  if(length(tmp))
    return(tmp)
  
  if(xmlName(doc) != "g" || substring(xmlGetAttr(doc, "clip-path"), 1, 9) != 'url(#clip')
     return(NULL)
  
  tmp = getNodeSet(doc, ".//x:path", "x", noMatchOkay = TRUE)

  if(addTypes) 
     sapply(tmp, setTypeAttr, "plot-point")
  
  tmp
}


getTextNodes =
  # 
function(doc, ...)
{
 getNodeSet(doc, "//x:g[count(./x:use) = count(./x:*)]", c(x = SVG.xmlns))
}

getTextPoints =
  # 
function(doc, ...)
{
 getNodeSet(doc, "//*[./s:use]",    #'//s:g[starts-with(s:use/@xlink:href, "#glyph2-")]',
                      c( s = SVG.xmlns, xlink="http://www.w3.org/1999/xlink"))

}


getViewBox =
function(doc)
{
  vb = getNodeSet(doc, "/x:svg/@viewBox", "x")[[1]]
  matrix(as.numeric(strsplit(vb, " +")[[1]]), 2, 2, byrow = TRUE)
}

isPlotRegion =
function(node, lattice = FALSE, hasAnnotations = lattice)
{
  ans = all (unique(names(node)) == "path")
  if(!ans && lattice && hasAnnotations)
              # the first few should be <path>.
    ans = min(which(names(node) != "path")) != 1
  
  ans
}

ncharTextNode =
function(x)
{
  sum(names(x) == "use") 
}

isPlotRegionNew =
function(node, lattice = FALSE, hasAnnotations = lattice)
{
  if(isPlotRegion(node, lattice, hasAnnotations))
    return(TRUE)

  gs = names(node) == "g"
  isText = sapply(xmlChildren(node)[gs], isTextNode)
  if(any(isText)) {
    nc = sapply(xmlChildren(node)[gs][isText], ncharTextNode)
    if(any(nc > 1))
      return(FALSE)
  }
  TRUE
}



getPlotRegionNodes =
  #
  #  Will handle mfrow arrangements
  #  What about pairs? Yes.
  #
  # If this is a lattice plot, then okay.
  #
  #
  #  But currently if there are any non standard pch points, it fails!
  #   This works now.
  # dd = svgPlot(plot(0:25, pch = 0:25))
  # rr = getPlotRegionNodes(dd)
  #
function(doc, lattice = FALSE, hasAnnotations = lattice,
             addTypes = TRUE, isRegion = isPlotRegionNew, ...)
{
   # see if it has been marked already.
  tmp = getNodeSet(doc, "//*[@type = 'plot-region']")
  if(length(tmp))
    return(tmp)

    # Get nodes that have a clip-path attribute as potential candidates.  
  tmp = getNodeSet(doc, "//x:g[@clip-path]", "x")
  i = sapply(tmp, isRegion, lattice, hasAnnotations)
  tmp = tmp[i]

   # Handle pairs() plots
  vb = getViewBox(doc) + c(0, 1, 0, 1)
  i = sapply(tmp, function(n) 
                    all(getPlotRegion(n) == vb))

  tmp =   tmp[!i]

    # If this is a lattice plot, we need to get rid of the
    # rectangles for panels.


  if(addTypes) {
     sapply(tmp, setTypeAttr, "plot-region")
  }
  
 tmp
}



isAnnotationGroup =
  #
  # Sees if the elements of g are all enclosed within the bounding box of 
  # parent 
function(g, parent, bbox = getBoundingBox(parent), scalar = TRUE)
{
  ans = xmlSApply(g, isInside, bbox)
  if(scalar)
    all(ans)
  else
    ans
}



# histogram
# <g clip-path="url(#clip3)" clip-rule="nonzero"> with all path childs
getPlotPoints.HistogramXMLPlot =
function(doc, simplify = TRUE, ...)
{
   gs = getNodeSet(doc, "//x:g", "x")
   i = sapply(gs, function(node) all (unique(names(node)) == "path"))

   ans = gs[i]
   if(length(ans) == 1)
     xmlChildren(ans[[1]])
   else
     unlist(lapply(ans, xmlChildren))
}


#################################################

getStyle =
function(node)
   UseMethod("getStyle")

getStyle.XMLInternalNode =
function(node)
  getStyle(xmlGetAttr(node, "style"))

getStyle.NULL =
function(node)  
  return(NULL)

getStyle.character =
function(node)  
{
  if((length(node) > 1 && length(names(node)) == length(node)) ||
         length(grep(":", node)) == 0)
     return(node)
  
  els = strsplit(gsub(";[[:space:]]*$", "", node), ";")[[1]]
  tmp = strsplit(els, ":[ ]*")
  structure(sapply(tmp, `[`, 2), names = trim(sapply(tmp, `[`, 1)))
}

setStyle =
function(node, ..., .style = structure(unlist(list(...)), names = names(list(...))))
{
   if(length(names(.style)) > 0)
      .style = paste(names(.style), .style, sep = ": ", collapse = ";")
   addAttributes(node, "style" = .style, append = TRUE)
}

            

modifyStyle =
function(style, ..., .vals = list(...))
  UseMethod("modifyStyle")

modifyStyle.XMLInternalNode =
function(style, ..., .vals = list(...))
{
  sty = xmlGetAttr(style, "style")
  sty = modifyStyle(sty, .vals = .vals)
  #xmlAttrs(style, append = TRUE) <- sty
  addAttributes(style, .attrs = c(style = sty), append = TRUE)
  style
}

modifyStyle.character =
function(style, ..., .vals = list(...))
{
  vals = getStyle(style)
  newVals = .vals

  vals[names(newVals)] = newVals
  paste(names(vals), vals, sep = ": ", collapse = "; ")
}  

###########################################


