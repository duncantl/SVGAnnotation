
compressStyles =
  #
  # threshold determines when we bother to convert the values
  #  NA means do it regardless
  #  0 < threshold < 1 => proportion of the length
  #  1 < threshold     => more than this number
  #
  # Need to be able to put these as path.classname
  #  Can we just have classname by itself.
  #
  #
function(doc, classNames = function(x)
                             paste("style", seq(along = x), sep = ""),
         threshold = NA,
         add = FALSE)
{
  sty = unlist(getNodeSet(doc, "//@style"))
  styles = factor(sty)
  usty = levels(styles)

     # determine whether it is worth compressing.
  if(!is.na(threshold)) {
    if(threshold < 1) {
      if(length(usty)/length(sty) > threshold)
             return(invisible(doc))
    } else {
      if(length(usty) > threshold)
          return(invisible(doc))
    }
  }
  
  nodes = getNodeSet(doc, "//*[@style]")
  defs = lapply(usty, getStyle)

  if(is.function(classNames))
    classNames = classNames(usty)
  else
    classNames = as.character(classNames)[1:length(usty)]
  
  mapply(function(node, styleName) {
                   removeAttributes(node, "style")
                   addAttributes(node, class = styleName)
                 }, nodes, classNames[styles])

  styles = structure(defs, names = paste(".", classNames, sep = ""))

  if(is.character(add)) {
      # Write the styles to this filename and
      # then add this
     txt = mapply(toCSS, styles, names(styles))
     cat(txt, file = add)
     addCSS(doc, add)
  } else if(is.logical(add) && add) {
      # Write the styles to a string and insert it into the document
     txt = mapply(toCSS, styles, names(styles))
     addCSS(doc, I(txt), insert = TRUE, silent = TRUE)
  } else
     styles
}

toCSS =
function(style, className, ...)
{
   paste(c(paste(className, "{") , paste(names(style),  style, sep = ": ", collapse = ";\n   "), "}"), collapse = "\n")
}




# Circles, text, rectangle for complex paths.


setGeneric("convertCSSStylesToSVG", 
           function(nodes, ...)
              standardGeneric("convertCSSStylesToSVG"))

setMethod("convertCSSStylesToSVG",  "XMLInternalDocument",
           function(nodes, ...) {
             convertCSSStylesToSVG(getNodeSet(nodes, "//*[@style]"))
             nodes
           })

setMethod("convertCSSStylesToSVG",  "list",
           function(nodes, ...) {
             invisible(lapply(nodes, convertCSSStylesToSVG, ...))
           })

setMethod("convertCSSStylesToSVG",  "XMLNodeSet",
           function(nodes, ...) {
             invisible(lapply(nodes, convertCSSStylesToSVG, ...))
           })


setMethod("convertCSSStylesToSVG",  "XMLInternalNode",
           function(nodes, ...) {
             val = getStyle(xmlGetAttr(nodes, "style"))
             if(length(val) > 0) {
                removeAttributes(nodes, "style")
                addAttributes(nodes, .attrs = val)
             }
               
           })    
