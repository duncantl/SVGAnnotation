getPanelDataNodes =
  #
  # For lattice.
  #
function(doc) {
   n = getNodeSet(doc, "//x:g[@clip-path]", "x")
   names(n) = sapply(n, xmlGetAttr, "clip-path")
   allPath = sapply(n, function(x) all(names(x) == "path"))

   tmp = names(n[allPath])
   ids = gsub("url\\(#(.*)\\)", "\\1", tmp)
   clipPaths = getNodeSet(doc, paste("//x:clipPath[@id = ", sQuote(ids), "]", collapse = "|"), "x")
   out = grep("^M 0 0", sapply(clipPaths, function(x) xmlGetAttr(x[[1]], "d")))
   n[ paste("url(#", sapply(clipPaths[-out], xmlGetAttr, "id") , ")", sep = "")]
}


addPanelText =
function(node, text)
{
  clip = gsub("url\\(#(.*)\\)", "\\1", xmlGetAttr(node, "clip-path"))
  doc = as(node, "XMLInternalDocument")
  data = getNodeSet(doc, paste("//x:clipPath[@id=", sQuote(clip), "]/x:path/@d"), "x")[[1]]
  data = getPath(data)
  rect = apply(data, 2, range)

  center = c(mean(rect[,1]), mean(rect[,2]))

  newSVGNode("text", attrs = c(x = center[1], y = center[2]), text, parent = node)
}

getPanelCoordinates =
function(doc = as(nodes[[1]], "XMLInternalDocument"), nodes = getPanelDataNodes(doc))
{
   ids = sapply(nodes, xmlGetAttr, "clip-path")
   i = gsub("url\\(#(.*)\\)", "\\1", ids)
   clips = getNodeSet(doc, paste("//x:clipPath[@id=", sQuote(i), "]/x:path/@d", collapse = "|"), "x")
   sapply(clips, getRect)
}




getStripNodes =
  #
  # Do we get the rectangle, the text, ...
  #
  # <g clip-path="url(#clip<n>)" clip-rule="nonzero"><path/></g> for the rectangle
  #
  # The text is in
  # <g style="...">
  # <use xlink:href="#glyph0-2" x="123.003906" y="100.466797"/>
  # </g>
  #
  # v = getNodeSet(doc, "//x:g[not(./x:g) and starts-with(@clip-path, 'url(#clip') and @clip-rule='nonzero']", "x")
  # v = getNodeSet(doc, "//x:g[not(./x:g) and count(./x:path) = 1 and starts-with(@clip-path, 'url(#clip') and @clip-rule='nonzero']", "x")
  # v = getNodeSet(doc, "//x:g[not(./x:g) and count(./x:path) = 1 and starts-with(@clip-path, 'url(#clip') and @clip-rule='nonzero' and not(contains(./x:path/@d, 'C'))]", "x")

  #
  #  ll = svgPlot(xyplot( mpg ~ wt | am + cyl, mtcars, group = carb))
  #  
  #
function(doc)
{
  # not(./x:g)
    # Perhaps we want to require the trailing/following rectangle.
  getNodeSet(doc, "//x:g[starts-with(@clip-path, 'url(#clip') and @clip-rule='nonzero' and (count(./x:path) = 2 or count(./x:path) = 1) and not(contains(./x:path/@d, 'C')) and ./x:g/x:use]", "x",
                noMatchOkay = TRUE)
}


getLatticeLegendNodes =
  #
  # Find the nodes in a lattive SVG document corresponding to the
  # (standard/default) key, i.e. with auto.key = TRUE or with options.
  #
function(doc, plotRegions = getPlotRegionNodes(doc), which = NA)
{
  if(TRUE) {
    ans = getLegendNodesDirect(doc)
    return(if(!is.na(which))
             ans[[which]]
           else
             ans)
  }
   
   types = getSVGNodeTypes(doc)
   if(length(types))
      return(xmlChildren(xmlRoot(doc)[["g"]])[ grep("^legend", types) ])
  
   strips = getStripNodes(doc)
   if(length(strips) == 0) {
      if(length(plotRegions) == 0)
         stop("Cannot find the plot region nodes when starting to look for the legend")
      legendNodes = getNodeSet(plotRegions[[length(plotRegions)]], "./following-sibling::*", noMatchOkay = TRUE)
      #legendNodes = other [ - (1:length(plotRegions)) ]
          # Arrange these into text, lines, etc.
   } else
      legendNodes = getNodeSet(strips[[length(strips)]], "./following-sibling::*", noMatchOkay = TRUE)[-1]


   info = getLegendInfo(doc) #XXX use this to count the number of nodes in each legend.
   if(!is.na(which)) {
   } else if(xmlName(info) == "multiLegend") {
   }
   
   legendNodes
}



isLatticePlot =
  #
  # Determine whether this plot is known to be a lattice/trellis plot.
  #
  #
function(doc)
{
   length(getLatticeInfo(doc)) > 0
}

getLatticeInfo =
  #
  # Get the SVG/XML node that we inserted into the SVG document
  # based on the lattice/trellis object used to create the plot.
  #
function(doc)
{
  o = getNodeSet(doc, "/x:svg/r:display/r:lattice", c("x" = SVG.xmlns, r = "http://www.r-project.org"))
  if(length(o))
     structure(o[[1]], class = c("LatticeLegendInfo", class(o[[1]])))
  else
    NULL
}


hasLegend =
  #
  # Determine if the SVG document has information about the legend
  # NA if we don't know and TRUE/FALSE if this is a lattice plot we
  # explicitly annotated at creation time.
  #
function(doc)
{
  if(!isLatticePlot(doc))
    return(NA)

  info = getLatticeInfo(doc)
  "legend" %in% names(info)
}

getLegendInfo =
  # Retrieve the legend information we inserted
  # into the SVG document when we created it based
  # on the lattice/trellis object.
function(doc)
{
  if(!isLatticePlot(doc))
    return(NA)

  info = getLatticeInfo(doc)
  if("multiLegend" %in% names(info))
     id = "multiLegend"
  else
     id = "legend"
  info[[id]]
}





isAxisNode =
  # Determine if this node represents a (lattice) axis with l
  # only horizontal lines  or
  #
  # This could detect a short vertical/horizontal line segment
  # inside the data region. So should check whether we are in a
  # plotting region.
  #
function(node)
{
  if(xmlName(node) != "g")
    return(FALSE)
  
  if(! all(names(node) %in% c("path", "g")) )
    return(FALSE)

  w = names(node) == "path"
  
  paths = node[w]

  if(length(paths) == 0)
    return(FALSE)
  
  for(i in paths) {
     p = xmlGetAttr(i, "d")
     if(length(grep("[CS]", p)) > 0)
       return(FALSE)
     gp = getGeneralPath(p)
     if(length(gp) > 2 || all(names(gp) != c("M", "L")))
       return(FALSE)
  }

  g = node[!w]
  if(length(g) == 0)
    return(TRUE)
  
  all(sapply(g, isTextNode))
}

isTextWrapperGNode =
function(x)
  xmlName(x) == "g" && !is.null(xmlGetAttr(x, "clip-path")) &&
       ( xmlSize(x) == 1 && all(names(x) == "g") && xmlName(x[[1]]) == "g" && all(names(x[[1]]) == "use")) 

isTextNode =
  #
  # Determine if this <g> node is really the equivalent of text.
  #
function(x)
{
  if(xmlName(x) == "title")
    return(TRUE)
  
  (xmlName(x) == "g" && xmlSize(x) > 0 && all(names(x) == "use") &&
     !any(is.na(xmlSApply(x, xmlGetAttr, "href", NA) ))) ||  isTextWrapperGNode(x)
}



missingTypes =
  #
  # What top-level nodes in the top-most <g> element do not have a type attribute,
  # i.e.  which ones did we miss.
  #
function(doc)
{
  els = getNodeSet(xmlRoot(doc)[["g"]], "./x:g[not(@type)]", "x")
  sapply(els, XML:::indexOfNode)
}

getSVGNodeTypes =
  #
  # get the type attribute for each of the top-level elements in the
  # top-most <g> element.
  #
function(doc)
{
  xpathSApply(xmlRoot(doc)[["g"]], "./*", xmlGetAttr, "type", NA, namespaces = c(x = SVG.xmlns))
}
