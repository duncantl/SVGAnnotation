addAxesLinks =
  #
  # Replacement that uses addLink
  #
function(doc, links, nodes = getAxesLabelNodes(doc), css = getDefaultSVGCSS(), style = "link")
{
  if(is.character(doc))
    doc = xmlParse(doc)

  if(missing(nodes) && is(doc, "XMLInternalNode") && length(links) == 1)
    nodes = doc
  
  ans = addLink(nodes, links, css = css, style = style, silent = missing(css))
#  addCSS(doc, css)  # don't add unless we succeed.
  invisible(ans)
}

rectAttrs =
function(box)
{
  if(!is.matrix(box))
    box = as(box, "RectangularBoundingBox")
  
  c(x = box[1,1], y = box[1,2],
    width = box[2,1] - box[1, 1],
    height = box[2, 2] - box[1, 2])
}

addLink =
function(n, url, addArea = NA, css = getDefaultSVGCSS(), style = "link", silent = FALSE, ...)
  UseMethod("addLink")


addLink.XMLInternalNodeList = addLink.XMLNodeSet = addLink.list = addLink.AxesLabelNodes =
function(n, url, addArea = NA, css = getDefaultSVGCSS(), style = "link", silent = FALSE, ...)
{
  doc = if(length(n)) as(n[[1]], "XMLInternalDocument") else NULL
  mapply(addLink, n, url, addArea, rep(NA, length(n)), style = style, 
            MoreArgs = list(doc = doc))
  if(length(css))
     addCSS(doc, css, silent = silent)
  
  invisible(n)
}

addLink.XMLInternalNode =
function(n, url, addArea = NA, css = getDefaultSVGCSS(), style = "link", silent = FALSE,
          doc = as(n, "XMLInternalDocument"), ...)
{
  isInlineStyle = is.na(style) || inherits(style, "AsIs") || length(grep("[:;]", style)) > 0
  attrs = if(is.na(style))
                c()
           else {
                if(isInlineStyle)
                   c(style = style)
                else
                   c(class = style)
             }

   if(isTextWrapperGNode(n))
      n = n[[1]]
  

  addArea = needToAddArea(n, addArea)
  
        # Create the link node with no parent or children.
        # Then replace the original node with this link and add it to the link node.
  if(addArea) {
     box = getBoundingBox(n)

     if(is(box, "Circle") || is(box, "Polygon")) {
        setBackgroundFill(n)
             # need to clone as otherwise, we add rect
             # to a the new node <a> and then we cannot replace
             # n with <a> as n is already replaced and in a.
        rect = xmlClone(n)
     } else {
        rect = newXMLNode("rect", namespaceDefinitions = SVG.xmlns,
                           attrs = c(rectAttrs(box), attrs, fill = getCanvasBackground(n)))
     }

     a = newXMLNode("a", rect, attrs = c("xlink:href" = as.character(url)),
                     namespaceDefinitions = c(xlink = "http://www.w3.org/1999/xlink"))
  } else {
     a = newXMLNode("a", attrs = c("xlink:href" = as.character(url), attrs),
                       suppressNamespaceWarning = TRUE,
                       namespaceDefinitions = c(xlink = "http://www.w3.org/1999/xlink"))
  }

  replaceNodes(n, a)
  addChildren(a, n)
  setSVGNs(a, doc)

  if(!isInlineStyle && length(css) > 0)
      addCSS(as(n, "XMLInternalDocument"), css, silent = silent)

  invisible(a)
}


cairoVersionExceeds =
function(ver)
{
  if(!is.na(cairoVersion)) {
     els = as.integer(strsplit(cairoVersion, ".", fixed = TRUE)[[1]])
     return(all(els[1:2] >= ver[1:2]))
  }

  NA
}



getAxesLabelNodes =
  #
  # For lattice plots, for example, we are missing the other labels by only taking the first one.
  #
function(doc, addTypes = TRUE)
{
  nodes = getNodeSet(doc, if(is(doc, "XMLInternalNode")) ".//*[@type='axis-label']" else "//*[@type='axis-label']")
  if(length(nodes))
     return(nodes)

  ver = cairoVersionExceeds(c(1, 8, 0))
  if(!is.na(ver) && ver)
    return(getNewAxesLabelNodes(doc))
  
  gs = getNodeSet(doc, "//x:g[starts-with(@clip-path, 'url(#clip')]", "x")
  use = sapply(gs, function(g) all(names(g) == "g") && all(xmlSApply(g, function(x) all(names(x) == "use"))))
     # Decompose these if we have a g that contains a g as the inner g's are the ones we want.
  ans = gs[use]



    # grz plots have a different structure than lattice plots
  if(length(ans) == 1 && all(names(ans[[1]]) == "g")) {
    ans = xmlChildren(ans[[1]])
  } else {
    class(ans) = "AxesLabelNodes"
  }

  if(addTypes)
     sapply(ans, setTypeAttr, 'axis-label')

  ans
}

getAxesLabelNames =
  #
  # Plotmath expressions cause some problems.
  #
function(nodes, doc)
{
  vb = getViewBox(doc)
  boxes = lapply(nodes, getBoundingBox)
#XXXX
  
  
}


isHorizontalText =
function(node)
{
  length(unique(xmlSApply(node, xmlGetAttr, "y", as.numeric))) == 1
}

isVerticalText =
function(node)
{
  length(unique(xmlSApply(node, xmlGetAttr, "x", as.numeric))) == 1
}

getNewAxesLabelNodes =
#
# This needs to also find a title if it is present.
#
#
function(doc, addTypes = TRUE)
{
  g = getNodeSet(doc, "/x:svg/x:g/x:g", "x")
  isText = sapply(g, function(x) all(names(x) == "use") && all(grepl("^#glyph", xmlSApply(x, xmlGetAttr, "href"))))

  if(!any(isText)) {

     nodes = getNodeSet(doc, "//x:g[ count(./x:g) = count(./x:*) ][count(./x:g/x:use) = count(./x:g/x:*)]", "x")
     if(length(nodes) == 0)
       return(list())

       g = unlist(lapply(nodes, xmlChildren))

  } else
     g = g[isText]

 
  hor = g[sapply(g, isHorizontalText)]
  vert = g[sapply(g, isVerticalText)]

     # Find a title if there is one.  
  plotRegion = getPlotRegionNodes(doc)
  plotRegion = plotRegion[[1]]
  data.box = getBoundingBox(plotRegion)
  bb = sapply(hor, function(node) {
                     bb = getBoundingBox(node)
                     bb[ 2 , "y"] < data.box[1, "y"] 
                    })
  if(any(bb)) {
    title = hor[bb] # [[1]] # could be multiple nodes?
    hor = hor[!bb]
    if(length(title) == 1)
      title = title[[1]]
  } else
    title = NULL

  x.pos = sapply(vert, function(x) xmlGetAttr(x[[1]], "x"))
  ynode = vert[[which.min(x.pos)[1]]]
  
  y.pos = sapply(hor, function(x) xmlGetAttr(x[[1]], "y"))
  xnode = hor[[which.max(y.pos)[1]]]

  ans = structure(list(xnode, ynode), class = "AxesLabelNodes")
  if(length(title))
    ans[[3]] = title

  ans
}

