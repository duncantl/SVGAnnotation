pathToCircle =
  #
  # Turn this generic path node into a circle
  #
  
function(path, box = getBoundingBox(path), force = FALSE, addTypes = TRUE)
{
       #XXX If the user has drawn something other than circles, we need to leave these.  
   if(force && !is(box, "Circle"))
      box = as(box, "Circle")
   
   if(is(box, "Circle")) {
                #
         box = as.numeric(box) # to remove the names!
         circ = newXMLNode("circle",
                           attrs = c(cx = box[1], cy = box[2], r = box[3], style = xmlGetAttr(path, "style"),
                                         if(addTypes) c(type = 'plot-point', class = "plot-point")))

         if(xmlSize(path))
            addChildren(circ, kids = xmlChildren(path))

         replaceNodes(path, circ)
         setSVGNs(circ)
         circ
    } else
       path
   
}   


getDefaultSVGCSS =
function(default = system.file("CSS", "RSVGPlot.css", package = "SVGAnnotation"))
{
   f = getOption("SVGCSS")  # In recent versions of R > 2.9.*, we can use getOption("SVGCSS", default)
   if(is.null(f))
     f = Sys.getenv("SVGCSS")
   if(f == "")
     f = default
   f
}

addCSS =
  #
  # Add a processing instruction or insert the content of the given CSS
  # file.
  #
function(doc, css = getDefaultSVGCSS(),
           insert = inherits(css, "AsIs"), silent = FALSE)
{
  if(length(css) == 0 || is.na(css) || nchar(css) == 0)
    return(invisible(doc))

  pis = getNodeSet(doc, "//processing-instruction('xml-stylesheet')[contains(., 'text/css')]", noMatchOkay = TRUE)
  if(!insert && length(pis) > 0) {
    vals = sapply(pis, xmlValue)
    if(!silent) {
       pat = sprintf('href *= *[\'"](file://)?%s', css)
       if(length(grep(pat, vals)) > 0) {
          warning("document already contains a processing instruction for this stylesheet ", css)
          return(invisible(doc))
       }
     }
  }


 if(insert) {
       name = NA
       if(!inherits(css, "AsIs")) {
          name = css
          css = readLines(css)
        }
       
       code = paste(c(if(!is.na(name)) paste("/* Contents of ", name, "*/"), css), collapse = "\n")
       node = newXMLNode("style", attrs = c(type="text/css"), newXMLCDataNode(code))
       addChildren(xmlRoot(doc), node, at = 0)
   } else  {
     if(length(grep("^(http[s]?|ftp):", css)) == 0)
         css = sprintf("'file://%s'", css)
     node = newXMLPINode("xml-stylesheet", paste('type="text/css" href=', css))
        # make certain to add the PI before the root <SVG> node or else Firefox, Safari, Chrome
        # won't use it.  Need very new version of XML (2.8.0)
     addSibling(xmlRoot(doc), node, after = FALSE)
   }


  invisible(doc)
}

getRect =
  #
  # Computes the bounding box/extremes of a path data string
  #  i.e. path@d
  #
function(str)
{
  dnames = list(c("start", "end"), c("x", "y"))
  
  if(length(str) == 0 || str == "")
     return(matrix(NA, 2, 2, dimnames = dnames))

   # Call getPath() for this, up to the range.
  els = strsplit(str, "[MLZCHhVv]")[[1]]
  con = textConnection(els)
  on.exit(close(con))
  els = matrix(scan(con, quiet = TRUE),, 2, byrow = TRUE)
  ans = apply(els, 2, range)
  dimnames(ans) = dnames
  ans
}


setIds =
  #
  # Put an id attribute on the specified nodes.
  # The idea is that we can do this within each plot region
  # to do simple linking.  And we would have an id that 
  # is the index of the observation but with a prefix identifying
  # the plot, e.g. plot1.3 for the third point in plot 1.
  #
  #
function(nodes, ids = seq(along = nodes), fix = c("", ""), sep = if(fix[1] == "") "" else "-")
{
   if(length(fix) > 0 ) 
      ids = paste(fix[1], ids, sep = sep)
   if(length(fix) > 1)
      ids = paste(ids, fix[2], sep = sep)     

   sapply(seq(length = min(length(nodes), length(ids))),
            function(i)
               addAttributes(nodes[[i]], "id" = ids[i], append = TRUE))

  invisible(ids)
}



# From RXMLDoc
trim =
function (x) 
{
    gsub("(^[[:space:]]+|[[:space:]]+$)", "", x, perl = TRUE)
}




sQuote =
function(x)
 paste("'", x, "'", sep = "")  


dQuote =
function(x)
 paste('"', x, '"', sep = "")



getTopG = getTopContainer =
function(doc)
{
  g = getNodeSet(doc, "//x:g[@id and starts-with(@id, 'surface')]", "x")
  if(length(g))
    g[[1]]
  else
    NULL
}





setTypeAttr =
function(node, value, addClass = TRUE)
{
   xmlAttrs(node) = c(type = value)
   if(addClass && is.null(xmlGetAttr(node, "class")))
     xmlAttrs(node) = c(class = value)
   node
}
