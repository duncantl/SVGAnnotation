getRange =
function(x, offset = .05)
{
  ans = range(x)
  ans + diff(ans) * c(-offset, offset)
}

transformX = function(vals, xlim, rect) {
#     vals = vals[!is.na(vals)]
     (vals - xlim[1]) * (rect[2, 1]- rect[1, 1])/(xlim[2] - xlim[1])  + rect[1, 1]
  }

  transformY = function(vals, ylim, rect) {
#    vals = vals[!is.na(vals)]    
    rect[1, 2] + (1 - (vals - ylim[1]) / (ylim[2] - ylim[1]) ) * (rect[2, 2] - rect[1, 2])
  }  

animate =
  #
  #  This is  currently low-level. We'll adapt it. See the original version in 
  #  ~/Books/XMLTechnologies/SVG/Animation/time.xml.
  #
  #  Take an existing plot and move the points for each level of which
  #
  # Let's assume we start with a stacked data frame  of (x, y, which)
  # values, or actually that x and y are grouped in data and which is a parallel
  # vector.
  # All the points for one value of which are contiguous
  #
  #
  #  Want a formula interface to identify the x and y and the time variable.
  # Also a group variable.  See the kmlFormula.
  #
function(file, data, which = if(ncol(data) > 2) data[,3]
                             else stop("you must specify which to identify the time steps"),
#          xlim = par("usr")[1:2],    #getRange(data[,1]), ylim = getRange(data[,2]),
#          ylim = par("usr")[3:4],
          xlim = NA, ylim = NA,
          interval = 2,
          begin = interval,
          dur = "10s",
          colors = character(),   # rainbow( numLevels )
          radii = list(),
          digits = 4,
          doc = xmlPlot(file),
          plotRegion = getPlotRegionNodes(doc)[[1]],
          points = getPlotPoints(plotRegion),
          dropFirst = TRUE,
          labels = character(),  labelInfo = list(pos = "center"),
          hook = NULL, ..., .args,
          addTypes = TRUE)
{
  
     # Get the doc into an XMLInternalDocument.
  if(is.character(file))
    doc = file = xmlParse(file)

  if(is(file, "XMLInternalDocument"))
    doc = file


      # We need the bounding box for this region.
  rect = getBoundingBox(plotRegion)  # getPlotRegion(plotRegion) # Should be getBoundingBox()?

  usr = getUSR(doc)
  if(length(xlim) == 1 && is.na(xlim))
    xlim = usr[1:2]
  if(length(ylim) == 1 && is.na(ylim))
    ylim = usr[3:4]  

     # Now get the time variable
  if(is.character(which) && length(which) == 1) {
    whichVar = which
    which = data[[which]]
  } else
    whichVar = character()

  which = factor(which)


    # If the caller wants us to drop the observations for the first  time period from the data,
    # we will do that.
  if(is.logical(dropFirst) && dropFirst)
    dropFirst = 1  # levels(which)[1]

  if(!is.logical(dropFirst)) {
      i = as.integer(which) != dropFirst 
      data = data[i, ] 

      if(length(whichVar))
        which = data[[whichVar]]
      else
        which = which[i]
  
      which = factor(which)      
  }


  data = do.call("rbind", by(data, which, function(x) x))


    # Okay, now we are (almost) ready to do the animation.
    # Figure out how many steps in the animation we have.
  numLevels = length(levels(which))
  numObs = length(points)

  if(is.character(radii)) 
    radii = by(data[, radii], rep(1:numObs, each = numLevels), function(x) x)


  idx = seq(0, by = numObs, length = numLevels)

  colors = as.character(colors)


  if(!missing(interval) && !missing(dur))
    stop("Can only specify interval or dur, but not both")

  if(!missing(interval)) {
    if(is.numeric(interval)) 
       unit = "s"
    else 
       unit = gsub("[0-9]", "", interval)

    dur = interval * numLevels
    attr(dur, "unit") = unit
    duration = paste(dur, "s", sep = "")
  } else {
    
       # Sort out the duration, allowing for a string with units or a number.
    if(is.numeric(dur)) {
      attr(dur, "unit") = "s"
      duration = paste(dur, "s", sep = "")
    } else {
      duration = dur
      dur = as.numeric(gsub("[a-zA-Z]", "", dur))
      attr(dur, "unit") = gsub("[0-9]", "", duration)
    }
  }


# starts = paste( seq(by = dur/numLevels, length = numLevels), attr(dur, "unit"), sep = "")
  starts = paste( seq(from = 0, by = dur/numLevels, length = numLevels) + begin, attr(dur, "unit"), sep = "")
  stageDur = paste( dur/numLevels, attr(dur, "unit"), sep = "")


if(FALSE) {  
  # Just checking we have the transformations correct. We draw lines from the center to each corner.
p1 = as.numeric(colMeans(rect))
p2 = as.numeric(transformX(xlim, xlim, rect))
p3 = as.numeric(transformY(ylim, ylim, rect))  
newXMLNode("line", attrs = c(x1 = p1[1], y1 = p1[2], x2 = p2[1], y2 = max(p3), stroke = "red"),  parent = plotRegion)
newXMLNode("line", attrs = c(x1 = p1[1], y1 = p1[2], x2 = p2[1], y2 = min(p3), stroke = "blue"),  parent = plotRegion)
newXMLNode("line", attrs = c(x1 = p1[1], y1 = p1[2], x2 = p2[2], y2 = min(p3), stroke = "green"),  parent = plotRegion)
newXMLNode("line", attrs = c(x1 = p1[1], y1 = p1[2], x2 = p2[2], y2 = max(p3), stroke = "orange"),  parent = plotRegion)    
}

  details = list(id = paste("move", 1:numLevels, sep = ""),
                 dur = rep(stageDur, numLevels),
                 start = starts,
                 interval = dur, begin = begin)


  ans = vector("list", numObs)
  for(i in seq(length = numObs)) {

            # Do this with getBoundingBox() which should return a Circle object.
        pt = points[[i]]

        box = getBoundingBox(pt)
        isCircle = is(box, "Circle")

        if(isCircle) {
           ans[[i]] = circ = pathToCircle(pt, box)
           cx = box[1]; cy = box[2]; cr = box[3]
        } else {
           ans[[i]] = circ = pt
           box = as(box, "RectangularBoundingBox")

           tmp = colMeans(box)
           cx = tmp[1]; cy = tmp[2]; cr = min(apply(box, 2, diff))/2
        }


       tmp.data = as.data.frame(lapply(data[idx + i, 1:2], fixNAs))
       
       x = round(transformX(tmp.data[, 1], xlim, rect), digits) 
       y = round(transformY(tmp.data[, 2], ylim, rect), digits)

            # first one we can infer from the actual location of the point.
       if(is.na(x[1])) x[1] = cx
       if(is.na(y[1])) y[1] = cy        

       path = paste(x, y, sep = ", ")
       path = c(paste(cx, cy, sep = ","), path)
       delta = paste(dx <- diff(c(cx, x)), dy <- diff(c(cy, y)), sep = ", ")

          # For non circles. Note that we use relative coordinates.
       from = c("0,0", paste(cumsum(dx), cumsum(dy), sep = ", "))

        
       if(begin > 0)  # have to make certain the points appear so force an animateMotion to the same location
                      # starting at time 0 and lasting 0 seconds.
          newSVGNode("animateMotion", 
                      attrs = c(# from = paste(cx, cy, sep = ","), to = paste(cx, cy, sep = ","),
                                to="0,0",
                                dur = "0s", begin = "0s", fill = "freeze"), 
                      parent = circ)
         
       for (j in 1:numLevels ) {

         if(isCircle) 
          newSVGNode("animateMotion", 
                      attrs = c(#from = path[j], 
                                #to = path[j+1],
                                # to = delta[j],
   from = from[j], to = from[j+1],
                #            path = sprintf("M %s L %s", path[j], path[j+1]),
                                id = details$id[j], 
                                dur = stageDur, begin = starts[j], fill = "freeze"), 
                          parent = circ)
         else {
          newSVGNode("animateTransform", 
                      attrs = c(from = from[j], to = from[j+1],
                                id = details$id[j], type = "translate",
                                attributeName = "transform",
                                dur = stageDur, begin = starts[j], fill = "freeze"), 
                      parent = circ)
        }

          d = list(id = details$id[j], dur = stageDur, begin = starts[j], index = i, step = j)          
          if(length(radii))
              animateRadius(circ, d, radii[[i]][j], from[j], from[j+1])        

          

          if(is.function(hook))  {

            if(length(.args) == 0)
               hook(circ, d, ...)
            else {
              e = quote(e(circ, d))
              e[[1]] = hook
              e[4:(3 + length(.args))] = .args
              eval(e, sys.frames()[[length(sys.frames())]])
            }
           }
       }

       if(length(colors))
         sapply(seq(along = colors), function(step) 
            {
               newSVGNode("set", 
                          attrs = c(attributeName = "fill", fill = "freeze", 
                                    attributeType="CSS", to = colors[step], 
                                    begin = paste("move", step, ".begin", sep="")), 
                          parent = circ)
            })

   }

   if(length(labels)) 
      animateBackgroundLabel(labels, details, plotRegion, info = labelInfo)


   invisible(list(circles = ans, details = details))
}

defaultLabelInfo = 
function(pos = as.numeric(colMeans(bbox)), region, bbox = getBoundingBox(region))
{
  attrs =  c('text-anchor' = "middle",
             'font-size' = 60, 
             fill = 'lightgray'
             )
  list(pos = pos, attrs = attrs)

}

mergeLabelInfo =
function(info, defaults)
{
   if(!("pos" %in% info))
     defaults$pos = info$pos

   ids = setdiff(names(info), "pos")
   defaults$attrs[ids] = unlist(info[ids])
   defaults$attrs[c("x", "y")] = defaults$pos
   defaults
}

animateBackgroundLabel =
function(labels, details, region, pos =  info[["pos"]],
         info = defaultLabelInfo(pos, region, bbox),
         id = 'backgroundText', bbox = getBoundingBox(region))
{
  if(is.character(pos)) {
     pos = mapPosToCoords(pos, bbox)
  }

  g = newSVGNode("g", parent = region, attrs = c(id = id), at = 0L)

  if(!missing(info))
    info = mergeLabelInfo(info, defaultLabelInfo(pos, region, bbox))


  start = details$start
  dur = details$dur
  if(length(labels) > length(start)) {
     start = c(details$begin, start)
     dur = c(dur[1], dur)
  }
  
  els = mapply(function(val, when, dur, to, freeze) {
                  text = newSVGNode("text", val, parent = g,
                                     attrs = c(x = pos[1], y = pos[2], info$attrs,
                                               visibility = if(to == "hidden") 'visible' else 'hidden'))
  
                  newSVGNode("set", attrs = c(attributeName = 'visibility', attributeType = "XML",
                                              to = to,
                                              if(to == 'visible') c(begin = when, dur = dur) else c(begin = when, dur = "0s"),
                                              if(freeze) c(fill = "freeze")),
                             parent = text)
                  text
       }, labels, start, dur,
          c("hidden", rep('visible', length(labels)-1)),
          c(TRUE, rep(FALSE, length(labels) - 2), TRUE))

  invisible(els)
}

mapPosToCoords = 
function(pos, bbox)
{
  if(length(pos) == 1)
     pos = rep(pos, 2)

  ans = numeric(2)
  ans[1] = if(pos[1] %in% c("centre", "center"))
               mean(bbox[,1])
           else if(pos[1] == "left")
               bbox[1,1]
           else if(pos[1] == "right")
               bbox[1,2]

  ans[2] = if(pos[2] %in% c("centre", "center"))
               mean(bbox[,2])
           else if(pos[2] == "top")
               bbox[1,2]
           else if(pos[2] == "bottom")
               bbox[2,2]

  ans
}


######################################

animateRadius =
  #
  # This version gets called numSteps times
  #  for each circle node, i.e. it is called for each stage/step
  # in the animation for each node. So lots!
  #
  # Each time it is called details is a list containing
  #   index - the index of the observation, 1, 2, ... n
  #   step - index of step/stage
  #   dur - the duration of this stage
  #   begin - the start time for this stage
  #   id - the id for this step.
  #
  # radius is an additional argument we pass to animate
  #
function(node, details, radius, from = NA, to = NA)
{

  if(xmlName(node) != "circle") {

    a = newSVGNode("animateTransform",
                           attrs = c(
                                     attributeName="transform" ,
                                     type="scale",
                                     fill = "freeze",
                                     to = radius, dur = details$dur, begin = details$begin),
                    parent = node)
    return(a)
  }

  
  r = radius * as.numeric(xmlGetAttr(node, "r"))  # for a hook function, use radius[[details$index]] as radius is a list for all points.

  newXMLNode("animate", attrs = c(attributeName = "r",
                                   fill = "freeze",
                                   attributeType = "XML",
                                   to = r, dur = details$dur, begin = details$begin), parent = node)
}


fixNAs =
  # fixNAs(c(  1, 2, NA, NA, 3))
  # fixNAs(c(1, 10, 11, NA, 3,NA))
  #
  # fixNAs(c(NA, 1, NA, 2, 3))
function(x)
{
  na = is.na(x)
  if(!any(na))
    return(x)
  
  i = which(na)
  pos = which(!na)

     # probably slow way of doing things.
#  x[i] = x[sapply(i, function(j) { tmp = x[1:(j-1)]; tmp = tmp[!is.na(tmp)]; tmp[length(tmp)]})]

  if(any(i < min(pos)))  # can't impute the first entry.
    i = i[ i > min(pos) ]

  if(length(i))
     x[i] = x[sapply(i, function(j) { pos[which.min( (j - pos) [(j - pos) > 0])]})]
  x
}






###############################################

       if (FALSE) { 
#DEB CHANGED
        path = paste("M", paste(x, y, collapse = " "))
        newXMLNode("animateMotion", 
                    attrs = c(path = path, fill = "freeze", dur = duration), 
                    parent = circ)

        if(length(colors))
          sapply(seq(along = colors)[-1], 
                function(step) {
                  newXMLNode("set",   
                    attrs = c(attributeName = "fill", fill = "freeze", 
                             attributeType="CSS", to = colors[step], 
                             begin = starts[step]), 
                    parent = circ)
                 })
         }
