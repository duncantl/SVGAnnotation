SVG.xmlns = "http://www.w3.org/2000/svg"

SVG.namespaces =
   c(xlink = "http://www.w3.org/1999/xlink",
     SVG.xmlns
     )

setGeneric("addECMAScripts",
   # Add the inclusion of the JavaScript code.  
function(doc, scripts, insertJS = inherits(scripts, "AsIs") || getOption("InsertScriptContents", FALSE), at = NA,
         ..., .jsvars = list(...), escapeFun = newXMLCDataNode)
            standardGeneric("addECMAScripts"))

setMethod("addECMAScripts", "character",
function(doc, scripts, insertJS = inherits(scripts, "AsIs") || getOption("InsertScriptContents", FALSE), at = NA,
         ..., .jsvars = list(...), escapeFun = newXMLCDataNode)
{
   addECMAScripts(xmlParse(doc), scripts, insertJS, at, ..., .jsvars = .jsvars, escapeFun = escapeFun)
 })


commentNodeWithSpace =
function(x, parent = NULL, doc = NULL, at = NA)
{
  newXMLCommentNode(sprintf("\n %s \n", as.character(x)), parent, doc, at)
}


setMethod("addECMAScripts", "HTMLInternalDocument",
function(doc, scripts, insertJS = inherits(scripts, "AsIs") || getOption("InsertScriptContents", FALSE), 
          at = NA, ..., .jsvars = list(...), escapeFun = commentNodeWithSpace)
{
   head = getNodeSet(doc, "/*/head", character())
   if(length(head) == 0)
      head = newXMLNode("head", parent = xmlRoot(doc), at = 0)
   else
      head = head[[1]]
  
   addECMAScripts(head, scripts, insertJS, at, ..., escapeFun = escapeFun, .jsvars = .jsvars)
})

setMethod("addECMAScripts", "XMLInternalDocument",
function(doc, scripts, insertJS = inherits(scripts, "AsIs") || getOption("InsertScriptContents", FALSE), at = NA,
         ..., .jsvars = list(...), escapeFun = newXMLCDataNode)
{
  addECMAScripts(xmlRoot(doc), scripts, insertJS, at, ..., .jsvars = .jsvars, escapeFun = escapeFun)
})

setMethod("addECMAScripts", "XMLInternalNode",
function(doc, scripts, insertJS = inherits(scripts, "AsIs") || getOption("InsertScriptContents", FALSE), at = NA,
         ..., isHTML = xmlName(doc) == "head", .jsvars = list(...), escapeFun = newXMLCDataNode)
{  
   insertJS = rep(insertJS, length = length(scripts))
   
   if(length(.jsvars)) {
      library(RJSONIO, pos = length(search()) - 1)
      jsNode = newXMLNode("script", attrs = c(type = "text/ecmascript"),
                                    namespaceDefinitions = SVG.namespaces, parent = doc, at = at)
      code = paste("var ", names(.jsvars), " = ", sapply(.jsvars, toJSON), ";", collapse = "\n\n", sep = "")
      if(!is.null(escapeFun))
         escapeFun(code, parent = jsNode)
      else
         addChildren(jsNode, code)
    }
   
   if(length(scripts) > 1) {
     sapply(seq(along = scripts),
              function(i)
                  addECMAScripts(doc, scripts[i], insertJS[i], at, escapeFun = escapeFun, isHTML = isHTML))
     return(invisible(doc))
   }

   if(length(scripts) == 0)
     return(invisible(doc))

      # Single script case
   
   jsNode = newXMLNode("script", attrs = c(type = "text/ecmascript"),
                                           namespaceDefinitions = SVG.namespaces)

   if(!is(scripts, "AsIs"))
      scripts = findJScripts(scripts, ok = TRUE)

   if(insertJS)  {
      code = if(!file.exists(scripts))
                 paste(scripts, collapse = "\n")
             else
                 paste(readLines(scripts, warn = FALSE), collapse = "\n")
      if(!is.null(escapeFun))
         escapeFun(code, parent = jsNode)
      else
         addChildren(jsNode, code)

      if(!is(scripts, "AsIs"))
        addAttributes(jsNode, "originalSource" = scripts)
   } else {
           #XXX may have to check if URL is not a local reference.
      if(length(grep("^(http[s]?|ftp):", scripts)) == 0)
          scripts = sprintf("file://%s", scripts)
      
      addAttributes(jsNode, "xlink:href" = scripts)  # normalizePath(path.expand(scripts)))
   }

     # If the caller has specified where to place this node, use that, otherwise
     # add it to the end of the existing script nodes, i.e. after the last existing <script>
   if(!is.na(at))
       addChildren(doc, jsNode, at = 0)
   else {
      others = if(isHTML) getNodeSet(doc, "./script") else getNodeSet(doc, "./x:script[@type='text/ecmascript']", "x")
      if(length(others))
         addSibling(others[[length(others)]], jsNode)
      else
         addChildren(doc, jsNode, at = 0)
   }

   invisible(doc)
})
