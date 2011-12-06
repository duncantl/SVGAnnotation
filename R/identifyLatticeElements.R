identifyLatticeElements =
  #
  # This function attempts to identify the elements of the SVG document (doc)
  # and put a type attribute on each that identifies  it in a meaningful manner.
  #
function(doc, plot = NULL)
{
   top = xmlRoot(doc)[["g"]]

   mapply(function(node, i) xmlAttrs(node) = c(count = i),
          xmlChildren(top), seq(length = xmlSize(top)))

   if(xmlName(top[[1]]) == "rect") {
     setTypeAttr(top[[1]], "canvas-background")
     offset = 0
   } else
     offset = -1

   if(!is.null(plot)) {

      hasXLab = hasYLab = FALSE
      if(length(plot$xlab) && (is.expression(plot$xlab) || (is.character(plot$xlab) && plot$xlab != ""))) {
          setTypeAttr(top[[2 + offset]], "x-axis-label")
          offset = offset + 1
          hasXLab = TRUE
      } else
         offset = offset - 1
      
      if(length(plot$ylab) && (is.expression(plot$ylab) || (is.character(plot$ylab) && plot$ylab != ""))) {
          tp = if(hasXLab) "y-axis-label" else "x-axis-label"
          setTypeAttr(top[[3 + offset]], tp)
          offset = offset + 1
      }  else
          offset = offset - 1     

   } else {
   
          #XXX may need to check if there is an xlab
      setTypeAttr(top[[2 + offset]], "x-axis-label")
      setTypeAttr(top[[3 + offset]], "y-axis-label")

      setTypeAttr(top[[4 + offset]], "y-axis")
      setTypeAttr(top[[5 + offset]], "x-axis")
    }

   plotRegions = getPlotRegionNodes(doc)
   identifyLatticePlotRegions(doc, plotRegions)
   
     # Find the strip nodes if there are any.
   strips = identifyLatticeStrips(doc, plot, plotRegions, top)

   identifyLatticeAxes(doc) #, plotRegions, strips)

   identifyLatticeLegend(doc, top, plot)
   
   invisible(doc)
}

identifyLatticeAxes =
function(doc)
{
     # In the original version, we had all the code for plot regions, strips, etc.
     # so we knew the nodes that we had already matched. So we could calculate their
     # indices and then work on those.
      #idx = c(if(length(strip)) sapply(strip, XML:::indexOfNode), if(length(plotRegions)) sapply(plotRegions, XML:::indexOfNode))
      # possibleAxes = xmlChildren(top)[ - idx ]
      possibleAxes = getNodeSet(xmlRoot(doc)[["g"]], "./x:*[not(@type)]", c(x = SVG.xmlns))
      sapply(possibleAxes, function(x)
                              if(isAxisNode(x))
                                  setTypeAttr(x, "axis"))   
}

identifyLatticePlotRegions  =
function(doc, plotRegions = getPlotRegionNodes(doc))
{
    # Find the plotting/data regions and annotate those
   invisible(sapply(plotRegions, function(x) {
                          setTypeAttr(x, "plot-region")
                          sib = getSibling(x)
                          setTypeAttr(sib, "plot-region-rectangle")  # or should we move it.                          
                       }))
}



identifyLatticeStrips  =
function(doc, plot, plotRegions = getPlotRegionNodes(doc), top = xmlRoot(doc)[["g"]])
{  
   strip = getStripNodes(doc)
   if(length(strip)) {

      newXMLNamespace(xmlRoot(doc), c(r = "http://www.r-project.org"))
     
      levels = getPanelLevels(plot)
      
      sapply(seq(along = strip),
          function(i) {
                    x = strip[[i]]
                    setTypeAttr(x,  "strip")
                    sib = getSibling(x)
                    setTypeAttr(sib, "strip-rectangle")  # or should we move it.

                    if(length(levels)) {
                      xmlAttrs(x, suppressNamespaceWarning = TRUE) = structure(as.character(levels[i,]), names = colnames(levels))
                    }
                  })

    }
    strip
}

getPanelLevels =
function(plot)
{
   if(!is.null(plot) && length(names(plot$condlevels))) { # if no conditioning still have condperms but with no name.

     # If we have shingle levels in the condlevels list, deal with these.
     labels = lapply(plot$condlevels[rev(plot$perm.cond)], as.character)
          # Want to use index.cond to rearrange within each element of perm.con
     if(length(labels) > 1) {
       levels = as.matrix(do.call("expand.grid", labels))
       i = rep(1:nrow(levels), each = length(plot$condlevels))
       levels = levels[ i, ]          
     } else {
       levels = matrix(labels[[1]], , 1, dimnames = list(NULL, names(labels)))
     }
     colnames(levels) = paste("r", colnames(levels), sep = ":")

   } else
     levels = matrix(0, 0, 0)

    levels
}  


getLegendNodesDirect =
  #
  # Returns the nodes associated with the legend(s) in this plot
  # assuming they are the last things drawn or that there are numAfter nodes following them.
  # 
  # This uses the legend information already written into the document
  # as a <r:legend>... or <r:multiLegend>..
  # and then determines the number of nodes in each and then
  # uses this to find the nodes.
  #
function(doc, info = getLegendInfo(doc), numAfter = 0)
{
   nodes = getNodeSet(doc, "//r:legend", c("r" = "http://www.r-project.org"))
   nels = sapply(nodes, numNodesInLegend)
   top = xmlRoot(doc)[["g"]]
   numNodes = sum(nels)
   lnodes = xmlChildren(top)[ - (1:(xmlSize(top) - numNodes - numAfter)) ][1:sum(nels)]

   if(length(nodes) == 1)
     lnodes
   else
     split(lnodes, rep(seq(along = nodes), nels))
}

numNodesInLegend =
  # takes an r:legend node with attributes telling us the number of
  # points, rectangles, etc.
function(node)
{
  atNames = c("numEntries", "points", "rectangles", "circles", "lines")
  ats = xmlAttrs(node)
  ats = ats[ atNames ]
  ats = ats[!is.na(ats)]
  sum(as.integer(ats), na.rm = TRUE)
}


identifyLatticeLegend =
function(doc, top = xmlRoot(doc)[["g"]], plot = NULL)
{
   if(is(doc, "XMLInternalDocument"))
     ilegend = getLegendInfo(doc)
   else
     ilegend = doc

   if(length(ilegend) == 0)
      return(FALSE)

   nodes = getLegendNodesDirect(doc)
   if(xmlName(ilegend) == "multiLegend") {
     sapply(seq(along = nodes),
             function(i)
                annotateLatticeLegend(doc, xmlChildren(ilegend)[[i]], nodes[[i]]))
   } else
     annotateLatticeLegend(doc, ilegend, nodes)
}

annotateLatticeLegend =
function(doc, ilegend = getLegendInfo(ilegend), nodes = getLegendNodesDirect(doc))
{
      num = xmlGetAttr(ilegend, "numEntries", , as.integer)
         # find out if which of points, rectangles and/or lines we have
      dec = xmlAttrs(ilegend)[c("points", "rectangles", "lines")]
      dec[is.na(dec)] = 0L
      dec = as.integer(sapply(c("points", "rectangles", "lines"),
                                 function(a) xmlGetAttr(ilegend, a, FALSE, getDecorationValue))) 
      numDecorations = sum(as.integer(as.logical(dec)))
      if(is.na(numDecorations)) {
        warning("Currently cannot process the legend for the lattice plots")
        return(FALSE)
      }
      nels = num * numDecorations


      mapply(function(node, labelNode) {
                   #XXX want to use setTypeAttr()
                 xmlAttrs(node) = c(type = "legend-label", class = "legend-label", value = xmlValue(labelNode))
             }, nodes[seq(length = num)], xmlChildren(ilegend))

      sapply(nodes[ - seq(length = num) ],
             function(node) {
                 setTypeAttr(node, "legend-glyph")
             })

    TRUE
}


getDecorationValue =
function(val)
{
  if(is.na(val))
    return(0)
  
  if(val == "TRUE")
    return(TRUE)
  if(val == "FALSE")
    return(FALSE)

  as.logical(as.integer(val))
}
