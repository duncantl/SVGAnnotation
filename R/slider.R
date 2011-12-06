addSlider =
  #
  #
  #  ... is for a list of JavaScript variables.
  #
function(doc, onload, javascript,
          id = "slider", svg = NULL,
          defaultJavascripts = c("mapApp.js", "helper_functions.js", "slider.js"),
          side = "bottom", #XXX support this adding space .
          ...)
{
  if(is.null(svg)) {
    if(is(doc, "XMLAbstractNode"))
       if(xmlName(doc) == "svg")
         svg = doc
       else
         doc = as(doc, "XMLInternalDocument")
    
     if(is.null(svg) && is(doc, "XMLInternalDocument"))
       svg = xmlRoot(doc)[["svg"]]
  }

  #XXX
  if(length(side) && side != "") {
    enlargeSVGViewBox(doc)
  }

     # Arrange to call the init() function to create the slider.
  addAttributes(svg, onload = onload) 
     # Put a node for the slider.    
  newSVGNode("g", attrs = c(id = "slider-lambda"), parent = svg)
     # Insert the Javascript code, including our own to provide the callback for the slider.
  addECMAScripts(doc, c(defaultJavascripts, javascript), TRUE, ...)

    # Now put the SVG to represent the thumb for the slider.
  sym = newSVGNode("symbol", attrs = c(id = "sliderSymbol",  overflow = "visible"),
                   parent = svg[["defs"]])
  newSVGNode("line", attrs = c(x1 = "0",  y1 = "-10",  x2 = "0",  y2 = "10",
                                         stroke = "dimgray", 'stroke-width' = "5",
                                         'pointer-events' = "none"),
              parent = sym)  

  invisible( doc )
}  
