linkPlots =
  #
  #  Given a collection of plots in the SVG which are assumed
  #  to have related observations, this provides simple linking
  #  whereby when we move the mouse over a point in one plot,
  #  the corresponding points in the other plots are also colored.
  #  When the viewer moves away from the point, the points revert to their
  #  original color.
  #
  #  In the future, we may add persistence, e.g. by clicking on the point
  #  it remains on even when we mouse out of the point.  But we need a way to
  #  reset. Not a big deal but pulls us away from our main goal here at present.
  #
  # insertJS allows one to control whether the code in script is inserted
  # directly into the document to make it self-contained.
  #
function(doc, col = "red", script = system.file("JavaScript", "link.js", package = "SVGAnnotation"), insertJS = TRUE)
{  
   addECMAScripts(doc, script, insertJS)

   reg = getPlotRegionNodes(doc)

   numPoints = sapply(reg, function(x) xmlSize(x))
   if(any(numPoints[1] != numPoints))
      stop("plots have different number of points")

   invisible(lapply(seq(along = reg),
                      function(i) {
                        points = xmlChildren(reg[[i]])
                        setIds(points, fix = c(paste("plot", i, sep = "")))
                        sapply(seq(along = points),
                               function(j)  {
                                 style = getStyle(points[[j]])
                                 idx = match("fill", names(style))
                                 addAttributes(points[[j]],
                                               onmouseover = paste("color_point(evt,", j, ",", length(reg), ",", sQuote(col), ")"),
                                               onmouseout = paste("reset_color(evt,", j, ",", length(reg), ")"),       
                                               fill = style["fill"],
                                               originalFill = style["fill"],
                                               style = paste(names(style)[-idx], style[-idx], sep = ":", collapse = ";"))
                        })
             }))

   invisible(doc)
}
