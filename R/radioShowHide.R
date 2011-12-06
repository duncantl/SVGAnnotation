radioShowHide =
  #
  # This uses the SVG widgets from Andreas Neumann to 
  # allow the viewer to toggle the elements
  # displayed in the viewing region of the plot.
  #
  #  within should be FALSE for a regular plot, TRUE for a matplot
  #  and not specified if group is TRUE.
  #
function(doc, insertScripts = TRUE, within = FALSE, group = FALSE,
          labels = paste("Series", seq(length = numSeries)),
          extraWidth = 15 * (max(nchar(labels)) + 1),
          save = if(!is(doc, "XMLInternalDocument")) doc,
          id.prefix = "series",
          checkboxCallback = if(is.na(within)) "togglePanel" else "toggle",
          jscripts = c("helper_functions.js", "timer.js", "checkbox_and_radiobutton.js", "hideSVGElements.js"),
          numPanels = 1)
{
  force(save)
  
  if(is.character(doc)) {
    doc = xmlParse(doc)
  }

  svg = xmlRoot(doc)
  
      # <svg> node
      # Change the viewbox and width on the svg
      # add the onload = "init()" attribute

      # Grab this so we know what the original dimensions were.
  vb = getViewBox(doc)
  enlargeSVGViewBox(doc, extraWidth)

  regions = getPlotRegionNodes(doc)
  numSeries = if(!missing(labels))
                  length(labels)
              else if(within)
                  sum(sapply(regions, xmlSize))
              else
                  length(regions)

  addAttributes(svg, onload = "init()")

  init.script = paste(
                  c("", "function init() {",
                  "var arr = new Array();",
                   paste("arr[", seq(along = labels) - 1, "] =", sQuote(labels),";", collapse = "\n"),
                   paste("createRadioBoxList(", length(labels), ",", vb[2,1], ", arr,", checkboxCallback, ",", sQuote(id.prefix), ",", numPanels, ");"),
                  "}"),
                  collapse = "\n")
  
     # Add definitions for the empty box and the X within it for the checkbox.
  addCheckboxDefinitions(doc)

     # Add  <g id="checkboxes"> node
  newSVGNode("g", attrs = c(id = "checkboxes"), parent = svg)

     # Add ids to each of the series.
  if(!is.na(within)) {
     ids = paste(id.prefix, seq(length = numSeries), sep = "")
     if(within) {
       objs = unlist(lapply(regions, xmlChildren), recursive = FALSE)
       sapply(seq(along = ids), function(i) addAttributes(objs[[i]], id = ids[i]))
          # Put a tooltip on each series.
       sapply(seq(along = ids), function(i) newSVGNode("title", labels[i], parent = objs[[i]]))
     } else
       sapply(seq(along = regions), function(i) addAttributes(regions[[i]], id = ids[i]))
   }
      # Add scripts
  addECMAScripts(doc, I(init.script), TRUE)
  scripts = findJScripts(jscripts)
  addECMAScripts(doc, scripts, insertScripts)

  if(is.logical(save) && save)
    save = docName(doc)
  
  if(is.character(save)) 
     saveXML(doc, save)
  else
     invisible(doc)
}

findJScripts =
  #
  #
function(scripts, dir = c(system.file("SVGGUI", package = "SVGAnnotation"),
                          system.file("JavaScript", package = "SVGAnnotation")),
          ok = FALSE, quiet = FALSE)
{
   i = file.exists(scripts)

   if(any(!i)) {
         w = which(!i)
         for(d in dir) {
               nf = paste(d, scripts[w], sep = .Platform$file.sep)
               e = file.exists(nf)
               if(any(e)) {
                  scripts[w[e]] = nf[e]
                  w = w[!e]
                  if(length(w) == 0)
                    break
               }
         }

         if(!quiet && length(w))
            (if(ok) warning else stop)("can't find ", paste(scripts[w], collapse = ", "))

if(FALSE) {         
         tmp = expand.grid(dir, scripts[!i])
         nf = paste(as.character(tmp[[1]]), as.character(tmp[[2]]), sep = .Platform$file.sep)
         e = matrix(file.exists(nf), , length(dir), byrow = TRUE)
         m = rowSums(e) == 0
         if(!quiet && any(m))
             (if(ok) warning else stop)("can't find ", paste(scripts[!i][m], collapse = ", "))
#         nf = paste(dir, scripts[!i], sep = .Platform$file.sep)
#         if(!quiet && any(f <- !file.exists(nf)))
#            (if(ok) warning else stop)("can't find ", paste(nf[f], collapse = ", "))
#      scripts[which(!i)[!f]] = nf
         if(!all(m))
            scripts[which(!i)[!m]] = apply(e[m,], 1, function(x) which(x)[1])
   }

   }
   scripts
}


enlargeSVGViewBox =
  #
  # Change the viewBox, width and height attributes of the root note.
  #
function(doc, x = 0, y = 0, svg = xmlRoot(doc))
{
  vb = getViewBox(doc)
  origWidth = vb[2,1]
  vb[2,1] = vb[2,1] + x
  vb[2,2] = vb[2,2] + y

  addAttributes(svg, width = vb[2,1], height = vb[2,2], viewBox = paste(t(vb), collapse = " "))

  invisible(doc)
}
  


addCheckboxDefinitions =
function(doc, svg = xmlRoot(doc))
{
      # Add checkBoxCross and checkBoxRect symbols to the defs
  defs = getNodeSet(doc, "//x:defs", "x")
  if(length(defs) == 0) 
    defs = newSVGNode("defs", parent = svg)
  else
    defs = defs[[1]]

# I took out the nested/inline newXMLNode() calls so that we can set the SVG namespace
# explicitly and avoid introducing new declarations.
  
   sym = newSVGNode("symbol", attrs = c(id="checkBoxRect", overflow="visible"),
            	     parent = defs)
   newSVGNode("rect", attrs = c(x = "-5", y = "-5",  width = "10", height = "10",
                                             fill = "white", stroke = "dimgray",
  	     	                             'stroke-width' = "1", cursor = "pointer"),
              parent = sym)

   sym = newSVGNode("symbol", attrs = c(id="checkBoxCross", overflow="visible"), parent = defs)
   tmp = newSVGNode("g", attrs = c('pointer-events' = "none",  stroke = "dimgray",  'stroke-width' = "1"), parent = sym)
   newSVGNode('line', attrs = c( x1 = "-3", y1 ="-3", x2 = "3", y2 = "3"), parent = tmp)
   newSVGNode('line', attrs = c( x1 = "3", y1 = "-3", x2 = "-3", y2 = "3"), parent = tmp)
   sym
}
