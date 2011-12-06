  #
  # A front-end to the svg() function in grDevices
  # that adds the R command that generates the plot
  # to the svg command.
  # This does not deal with subsequent annotations of the
  # plot. As a result, one is encouraged to put all of the
  # plot commands within { ... } as a complete block.
  #
  # Adding the R command(s) to the SVG file allows the
  # post-processing facilities to attempt to make sense of the elements
  # of the plot(s).  Additionally, it is feasible (though difficult to be done generally)
  # for this function to be extended to annotate information about the source of the data
  # and the limits used in the plot. But this will use very non-standard evaluation.
  #
  # For example, a call of the form
  #  with(data, plot(y ~ x, xlim = range(x), ylim = range(y), type = "n", xlab = "X", ylab = "Y"))
  # we can decompose the expression looking for the xlim, ylim parameters and if we find either,
  # evaluate that in the parent frame to get the limits.
  # It is much more desirable to use a grid/lattice-like display where we can ask for this
  # information after the plot is created.
  #

setOldClass(c("SVGDocument", "XMLInternalDocument", "XMLAbstractDocument"))


dim.SVGDocument =          
 function(x) {
   r = getNodeSet(x, "//x:svg", "x")[[1]]
   tmp = c(xmlGetAttr(r, "width"), xmlGetAttr(r, "height"))
   units = gsub("[0-9.]", "", tmp)
   structure(as.numeric(gsub("[^0-9.]", "", tmp)), names = units)
 }

getCSSPIRef =
function(x) {
  tmp = strsplit(xmlValue(x), "[[:space:]=]+")[[1]]
  i = match("href", tmp)
  tmp[i + 1]
}  

getCSS =
function(doc, asNodes = FALSE)
{
  if(is.character(doc))
     doc = xmlParse(doc)
     
  pis = getNodeSet(doc, "//processing-instruction('xml-stylesheet')[contains(., 'text/css')]", noMatchOkay = TRUE)
  inline = getNodeSet(doc, "//style[@type='text/css']", noMatchOkay = TRUE)

  if(asNodes)
    return(c(pis, inline))
  
  if(length(pis))
    ans = sapply(pis, getCSSPIRef)
  else
    ans = character()
  

  c(ans, sapply(inline, xmlValue))
}


getJavaScript = getECMAScript =
function(doc, asNodes = FALSE)
{
   if(is.character(doc))
     doc = xmlParse(doc)

   
    # XXX should look at the attribute on <svg> named contentScriptType
    # as that gives the default type. So if a script element has no type attribute, we would include
    # that assuming there is no contentScriptType attribute on <svg> or it is set to one of the ones identifying
    # ECMA/Java-Script
  inline = getNodeSet(doc, "//x:script[@type='text/javascript' or @type='text/ecmascript' or not(@type)]", "x", noMatchOkay = TRUE)

  if(asNodes)
    return(inline)
   
       # Could put the names on the elements which have a src but which inline the file's contents.
  sapply(inline, function(x) {
                             val = xmlGetAttr(x, "href", NA)
                             if(is.na(val))
                                  xmlValue(x)
                             else
                                 val
                            })
}



#setMethod("svgPlot", c(cmd = "ANY"),
svgPlot =
function(cmd, filename = paste(tempfile(), "svg", sep = "."), ...,
         asXML = missing(filename), print = inherits(ans, "trellis"),
         removeFile = asXML, addInfo = TRUE)
{
  off = FALSE
  # onWindows = Sys.info()["sysname"] == "Windows"
  #if(useCairoPkg) {
  #  library(Cairo)
  #  CairoSVG(filename, ...)
  #} else
  #   grDevices::svg(filename, ...)

  svgFun(filename, ...)
  
  on.exit({if(!off) dev.off(); if(removeFile) unlink(filename)})
  tmp = substitute(cmd)
  txt = if(class(tmp) == "{")
           paste(c(as.character(tmp), "}", ""), collapse = "\n")
        else
           paste(deparse(tmp), collapse = "\n")

  ans = cmd
  if(print)
     print(ans)

  usr = par("usr")
  dev.off(); off = TRUE
  
  doc = xmlParse(filename)
  class(doc) = c("SVGDocument", class(doc))  
  rg = newXMLNode("r:display", newXMLCDataNode(txt),
                   namespaceDefinitions = c("r" = "http://www.r-project.org"),
                   parent = xmlRoot(doc), at = 0,
                   attrs = c(usr = paste(usr, collapse = ",")))

  if(useCairoPkg)
    addCanvasRect(doc)


  if(addInfo && is(ans, "trellis"))  {
     addPlotInfo(ans, rg)
     identifyLatticeElements(doc, ans)
  }

  if(removeFile)
      unlink(filename)
  
  if(asXML) {
    doc
  } else {
     saveXML(doc, filename)
     filename
  }
}


addCanvasRect =
function(doc)
{
   dim = as.numeric(dim(doc))
   r = newSVGNode("rect", attrs = c(x = 0, y = 0, width = dim[1], height = dim[2],
                                    style="fill: rgb(100%,100%,100%); fill-opacity: 1; stroke: none;"),
                   parent = xmlRoot(doc)[["g"]], at = 0)
   r
}

newSVGNode =
function(...)
{
  node = newXMLNode(...)
  setSVGNs(node)
  node
}

setSVGNs =
  #
  # put the namespace from the root node on the new node.
  #
function(node, doc = as(node, "XMLInternalDocument"), namespace = SVG.xmlns)
{
   if(is.null(doc)) {
     XML:::setXMLNamespace(node, namespace)
   } else {
     nsDefs = XML:::namespaceDeclarations(xmlRoot(doc), TRUE)
     XML:::setXMLNamespace(node, nsDefs[[1]])
   }
   node
}


addPlotInfo =
function(obj, node, ...)
  UseMethod("addPlotInfo")

addPlotInfo.trellis =
  #
  # Adds meta data to the SVG document for a lattice/trellis plot.
  #
function(obj, node, ...)
{

   ylims = if(is.numeric(obj$y.limits)) obj$y.limits else numeric()
   lims = structure(as.numeric(c(obj$x.limits, ylims)),  # get rid of the class info just in case we have a POSIXt here and adding the lims["panelFunction"]
                                             # will raise an error because of [.POSIXt/POSIXct.
                     class = "numeric", 
                     names = paste(c("x", "x", if(length(ylims)) c("y", "y")), rep(c("min", "max"), if(length(ylims)) 2 else 1), sep = "."))

   if( is.character(obj$panel))
     lims["panelFunction"] = obj$panel
  
  lat = newXMLNode("r:lattice",
                   attrs = c(numPanels = length(obj$panel.args),

                             lims),
                           #  numXTickMarks = obj$x.scales$tick.number,  # if we have different scales on different panels, we need to do this differently.
                           #  numYTickMarks = obj$y.scales$tick.number),
                   parent = node)

  
  
  sapply(obj$panel.args,
          function(x) {
            newXMLNode("panelIndices", paste(x$subscripts, collapse = ","), parent = lat)
          })

  if(length(obj$condlevels) > 1) {
    ns = prod(sapply(obj$condlevels, length)) * length(obj$condlevels)
    cond = newXMLNode("conditioning", attrs = c(numFactors = length(obj$condlevels), numStrips = ns), parent = lat) 
    mapply(function(id, m) {
             newXMLNode("factor", attrs = c(numLevels = nrow(m), name = id), parent = cond)
          }, names(obj$condlevels), obj$condlevels)
  }

  if(!is.null(obj$legend)) {
     if(length(obj$legend) > 1) 
        p = newXMLNode("multiLegend", parent = lat, namespace = "r", namespaceDefinitions = c(r = "http://www.r-project.org"))
     else
        p = lat

     mapply(makeXMLLegendInfo, obj$legend, names(obj$legend), MoreArgs = list(parent = p))
  }


     #Serialize the lattice object and put it at the bottom of the file.
  con = textConnection(NULL, "w", local = TRUE)
  on.exit(close(con))
  dput(obj, con)
  serialized = textConnectionValue(con)

  newXMLNode("object", newXMLCDataNode(serialized), attrs = c(id = "latticeObject"),
             parent = xmlRoot(as(lat, "XMLInternalDocument")),
              namespace = "r", namespaceDefinitions = c(r = "http://www.r-project.org"))

  invisible(lat)
}


makeXMLLegendInfo =
function(obj, space = NA, parent = NULL)
{
     if(all(c("fun", 'args') %in% names(obj)))
         return(makeXMLLegendInfo(obj$args, space, parent))

     if("args" %in% names(obj))
       key = obj$args
     else
       key = obj
     

     if("key" %in% names(key)) {
         key = key$key
         labels = if("lab" %in% names(key$text))
                     key$text$lab
                  else if(length(names(key$text)) == 0) # just text and no name elements
                     unlist(key$text)
     } else
         labels = key$text

     legendAttrs = c(numEntries = length(labels))

    
     ats = c("points", "rectangles", "lines", "columns", "space")
     i = ats %in% names(key)
     legendAttrs[ats[i]] = sapply(ats[i], function(x) {
                                             val = key[[x]]
                                             if(is.list(val))
                                               max(sapply(val, length))
                                             else if(is.logical(val))
                                               as.character(if(val) length(labels) else "")
                                             else
                                               as.character(val)
                                           })
    legendAttrs = legendAttrs[ legendAttrs != "" ]
     
#                     structure(unlist(tmp), names = names(tmp)))
     leg = newXMLNode("legend", attrs = legendAttrs, parent = parent,
                         namespace = "r", namespaceDefinitions = c(r = "http://www.r-project.org"))
     sapply(labels,
              function(x) newXMLNode("label", x, parent = leg, namespace = "r"))

     leg
}


svgPlot.ggplot =
function(filename, cmd, ..., asXML = FALSE)
{
  grDevices::svg(filename, ...)
  print(cmd)
  dev.off()

  doc = xmlParse(filename)
  
  if(asXML)
    doc
  else {
     saveXML(doc, filename)
     filename
  }  
}

#setMethod("svgPlot", c(cmd = "trellis"),
svgPlot.trellis = 
function(filename, cmd, ..., asXML = FALSE)
{
  grDevices::svg(filename, ...)
  print(cmd)
  dev.off()
  tmp = cmd$call
  txt = if(class(tmp) == "{")
           paste(c("{", as.character(tmp), "}", ""), collapse = "\n")
        else
           deparse(tmp)          
  doc = xmlParse(filename)
  newXMLNode("r:display", newXMLCDataNode(txt),
              namespaceDefinitions = c("r" = "http://www.r-project.org"),
              parent = xmlRoot(doc), at = 0)

  if(asXML)
    doc
  else {
     saveXML(doc, filename)
     filename
  }
}


getLatticeObject =
function(doc, asNodes = FALSE)
{
  if(is.character(doc))
    doc = xmlParse(doc)

  nodes = getNodeSet(doc, "/*/r:object[@id = 'latticeObject']", c(r = "http://www.r-project.org"))
  if(asNodes)
    return(nodes)
  
  invisible(eval(parse(text = xmlValue(nodes[[1]]))))
}


getUSR =
function(doc)
{
  els =  getNodeSet(doc, "/*/r:display/@usr", c(r = "http://www.r-project.org"))
  as.numeric(strsplit(unlist(els), ",")[[1]])
}



getRCommand =
  #  library(vcd)
  #  sex = svgPlot(sex.table <- mosaic(~ ExtramaritalSex + PremaritalSex | MaritalStatus + Gender, data = PreSex))
  #  class(eval(SVGAnnotation:::getRCommand(sex)[[1]][[3]][[2]]))
  #
function(doc)
{
   d = getNodeSet(doc, "//r:display", c(r = "http://www.r-project.org"))
   if(length(d))
     parse(text = xmlValue(d[[1]]))
   else
     NA
}
