addToolTips =
function(file, text = seq(along = paths), which = seq(along = text), doc = xmlPlot(file), save = FALSE,
          paths = getPlotPoints(doc), elName = "title", addArea = NA, style = "tooltip",
          addTitleAttribute = TRUE, addCSS = NA, silent = missing(addCSS), ...)
   UseMethod("addToolTips")


addToolTips.XMLInternalDocument =
function(file, text = seq(along = paths), which = seq(along = text), doc = xmlPlot(file), save = FALSE,
          paths = getPlotPoints(doc), elName = "title", addArea = NA, style = "tooltip",
          addTitleAttribute = TRUE, addCSS = NA, silent = missing(addCSS), ...)
{

  if(missing(doc))
     doc = file
   # If there are multiple sets of plot points returned by getPlotPoints()
   # then we see if one of them has the same number of elements as in text.
   # Assumes the caller specified text.
  if(class(paths) == "list" && all(sapply(paths, is, "XMLNodeSet"))) {
     len = sapply(paths, length)
     if(any(w <- (len == length(text))))
        paths = paths[[ which(w)[1] ]]
  }

  if(is.na(addArea) || addArea > 0)  
    addToolTips(paths, text, which, doc, save, paths, elName, addArea, style, addTitleAttribute, FALSE, addCSS = FALSE, silent = silent, ...)
  else
    invisible(sapply(seq(along = which),
                     function(i) {
                        tmp = paths[[ which[i] ]]
                        newSVGNode(elName, text[which[i]],  parent = tmp)
                        if(addTitleAttribute)
                           xmlAttrs(tmp, suppressNamespaceWarning = TRUE) = c('xlink:title' = text[which[i]])
                     }))

  if((is.na(addCSS) && (is.na(addArea) || addArea > 0))  || addCSS)
    addCSS(doc, silen = silent)

  if(is.logical(save) && save)
    save = file
  
  if(is.character(save)) {
     saveXML(doc, if(is.character(save)) save else file)
     save
  } else
    invisible(doc)
}

addToolTips.character =
function(file, text = seq(along = paths), which = seq(along = text), doc = xmlPlot(file), save = FALSE,
          paths = getPlotPoints(doc), elName = "title", addArea = NA, style = "tooltip",
          addTitleAttribute = TRUE, addCSS = NA, silent = missing(addCSS), ...)
{
   invisible(addToolTips(doc, text, which, doc, save, paths, elName, addCSS = addCSS, silent = silent, ...))
}


addToolTips.XMLPlot =
  #
  #  addTooltips("occ.xml", save = "occ_tips.xml")
  # load("/Users/duncan/Classes/StatComputing/DebLectures/Stat133/lectures/Traffic/myTraffic.rda")
  #  traffic$time = as.POSIXct(as.Date("02/27/2004 00:01", "%m/%d/%Y %H:%S")) + 
  #                               seq(0, by = 5*60, length = nrow(traffic))
  #  addTooltips("occ.xml", save = "occ_tips.xml")
  #
  #
  #  For a histogram, we need to color the rectangles to make them "active".
  #
function(file, text = seq(along = paths), which = seq(along = text), doc = xmlPlot(file), save = FALSE,
          paths = getPlotPoints(doc), elName = "title", addArea = NA, style = "tooltip",
          addTitleAttribute = TRUE, addCSS = NA, silent = missing(addCSS), ...)
{  
  invisible(addToolTips(doc, text, which, doc, save, paths, elName, addArea, style, addTitleAttribute, addCSS, silent = silent, ...))
}

addToolTips.HistogramXMLPlot =
function(file, text = seq(along = paths), which = seq(along = text), doc = xmlPlot(file), save = FALSE,
          paths = getPlotPoints(doc), elName = "title", addArea = NA, style = "tooltip",
          addTitleAttribute = TRUE, addCSS = NA, silent = missing(addCSS), ...)
{

     #XXX Shouldn't use white, but the actual background.
   sapply(paths, modifyStyle, fill = "white")
   NextMethod("addToolTips")
}

needToAddArea =
function(node, addArea)
{  
   if(is.na(addArea)) {
      type = xmlGetAttr(node, "type", "")
      if(type %in% c("strip"))
        addArea = FALSE
      else if(type %in% c("plot-region"))
        addArea = TRUE
      else if(isTextNode(node))
        addArea = TRUE
      else {
        if(xmlName(node) == "path" && xmlSize(node) == 0 &&
             getStyle(node)["fill"] == "none")
          return(TRUE)
        
        addArea = xmlSize(node) > 0 && all(names(node) == "use")
      }
   }
   addArea
}

addToolTips.XMLInternalNode =
function(file, text = seq(along = paths), which = seq(along = text),
          doc = as(file, "XMLInternalDocument"), save = FALSE,
          paths = getPlotPoints(doc), 
          elName = "title", addArea = NA, style = "tooltip",
          addTitleAttribute = TRUE, addCSS = NA, silent = missing(addCSS), ...) 
{
   addArea = needToAddArea(file, addArea)
  
   if(addArea > 0) {
       # add an "invisible" rectangle after the node so that
       # we don't get problems with the tooltip only applying
       # to the path when we are drawing letters via a path.

      ans = addAreaTooltip(file, text, style, elName)
      if(addArea > 1) {
        a = newXMLNode("a", attrs = c('xlink:title' = text), suppressNamespaceWarning = TRUE)
        replaceNodes(file, a)
        addChildren(a, file)
        if(is.na(addCSS) || addCSS)
          addCSS(as(a, "XMLInternalDocument"), silent = silent)
      }
   } else {
         # otherwise, just add a <title> node.
      ans = newSVGNode(elName, text, parent = file)
      if(addTitleAttribute)
         xmlAttrs(file, suppressNamespaceWarning = TRUE) = c('xlink:title' = text)
   }

   
   if(is.logical(save) && save)
     save = docName(doc)
  
   if(is.character(save)) {
     saveXML(as(file, "XMLInternalDocument"), if(is.character(save)) save else file)
     save
   } else
    invisible(doc) # invisible( doc )
}


addToolTips.list = addToolTips.XMLInternalNodeList = addToolTips.XMLNodeSet = addToolTips.AxesLabelNodes =
function(file, text = seq(along = paths), which = seq(along = text),
          doc = as(file, "XMLInternalDocument"), save = FALSE,
          paths = getPlotPoints(doc), 
          elName = "title", addArea = xmlSize(file) > 0 && all(names(file) == "use"),
          style = "tooltip",
          addTitleAttribute = TRUE, addCSS = NA, silent = missing(addCSS),
          ...) 
{
    mapply(addToolTips, file, text, MoreArgs = list(addArea = addArea, style = style, elName = elName, addTitleAttribute = addTitleAttribute, addCSS = FALSE))

    if(length(file) > 0 && ( is.na(addCSS) || addCSS))
      addCSS(as(file[[1]], "XMLInternalDocument"), silent = silent)
    
    invisible(file)
}

addAreaTooltip =
function(file, text, style = "tooltip", elName = "title", addTitleAttribute = TRUE)
{
     if(!is.na(xmlGetAttr(file, "clip-path", NA)) && xmlSize(file) == 1 && names(file) == "g")
                   file = file[[1]]

     parent = xmlParent(file)     

     box = getBoundingBox(file)

     isInlineStyle = is.na(style) || inherits(style, "AsIs") || length(grep("[:;]", style)) > 0
     attrs = if(is.na(style))
                c()
             else {
                if(isInlineStyle)
                   c(style = style)
                else
                   c(class = style)
             }


     if(is(box, "Circle") || is(box, "Polygon")) {
        # We may want to set the circle's fill style to the background color
        # rather than draw a circle ourselves.
        # We can get the background color from the top-level <rect>
        # (at least with the libcairo-based svg() command. Not the Cairo device)
        #
        # Alternatively, we can add a new circle.  I think I like the conditionally
        # coloring the interior better.
       
        setBackgroundFill(file)
        newSVGNode(elName, text, parent = file)
        if(addTitleAttribute)
           xmlAttrs(file, suppressNamespaceWarning = TRUE) = c('xlink:title' = text)
        
        if(FALSE)
          newSVGNode("circle", newXMLNode(elName, text), 
                    attrs = c(cx = box[1], cy = box[2], r = box[3],
                              attrs), 
                    parent = parent)
     } 
     else {
        if(addTitleAttribute)
          parent = newXMLNode("a", parent = parent, attrs = c('xlink:title' = text))
        
        r = newSVGNode("rect",  
                        attrs = c(rectAttrs(box), attrs),
                         parent = parent)
        newSVGNode(elName, text, parent = r)
        if(addTitleAttribute)
           xmlAttrs(file, suppressNamespaceWarning = TRUE) = c('xlink:title' = text)        
        r
     }
}


setBackgroundFill =
  #
  # Change the fill value of the specified node, but only if it is not
  # set or is explicitly "none".
  # This is for having mouse events in the interior of the node.
  #
  #
function(node, fill = NA, isCSS = NA)   # whether this is a CSS style or an XML attribute style.)
{
  sty = getStyle(node)

  if(is.null(sty)) {
     isCSS = FALSE
     cur = xmlGetAttr(node, "fill", NA)
  } else {
     cur = sty["fill"]
     isCSS = TRUE
  }
  
  if(!is.na(cur) && cur != "none")
    return(FALSE)
  
  fill = getCanvasBackground(node)
  
  if(isCSS) {
     sty["fill"] = fill
     setStyle(node, .style = sty)
  } else
     addAttributes(node, c(fill= fill))
}

getCanvasBackground =
function(node)
{
  if(!is(node, "XMLInternalDocument"))
     node = as(node, "XMLInternalDocument")
  r = getNodeSet(node, "/x:svg/x:g/x:rect", "x")

  if(length(r) == 0) {
     # For Cairo device.
  }

  as.character(getStyle( r[[1]] )["fill"]) # get rid of the names.
}
