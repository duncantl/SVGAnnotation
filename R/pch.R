numPathElementsPerPCH =
  # See tests/shapes.R
  # This gives the number of path elements that are generated for a
  # single observation/point in a plot for each of the different
  # values of pch.
structure(c(1L, 1L, 1L, 2L, 2L, 1L, 1L, 3L, 4L, 3L, 3L, 2L, 3L, 
3L, 2L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L), .Names = c("0", 
"1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", 
"13", "14", "15", "16", "17", "18", "19", "20", "21", "22", "23", 
"24", "25"))


groupByPch =
  #
  #  this function looks at the different pch values and 
  # determines which path elements in nodes correspond to
  #  which pch and groups them. The need for this is because
  #  10 of the pch values give rise to more than one path node
  #  in the SVG output.
  #
function(nodes, pch)  
{
      # We could allow a list here.
   if(is.character(pch)) {
     if(any(pch %in% as.character(0:25)))
        warning("R graphics cannot mix integer and character plotting characters (pch) and preserve the integer meaning (all are coerced to character). So all nodes would be plotted using the character, not the special symbols. Leaving the nodes as they are.")
     return(nodes)
   }
   
   isPath = sapply(nodes, xmlName) == "path"

   pnodes = nodes[isPath]  #??? do we use this anymore?
   
   pch = as.character(pch)  # so can use to index numPathElementsPerPCH

     # identify which nodes go together by creating
     # an index for each group where if, e.g.,
     # a pch value has 3 nodes, we repeat the index 3 times

        # this version handles recycling.
   e = numPathElementsPerPCH[rep(as.character(pch), length = length(nodes))]
   num = rep(seq(along = nodes), e)
   length(num) = length(nodes)
     # put the pch from which each node comes.
   tmp = rep(names(e), e)
   length(tmp) = length(num)
   names(num) = pch = tmp


   parent = xmlParent(nodes[[1]])
   doc = as(parent, "XMLInternalDocument")
   D = data.frame(idx = seq(along = nodes), pch = pch, stringsAsFactors = FALSE)
   ans = by(D, num,
                 function(d) {
                     x = nodes[ d[, 1]]
                     if(length(x) > 1) {
                          pos = XML:::indexOfNode(x[[1]])
                          removeNodes(x)
                          sapply(x, removeAttributes, .attrs = "type") # remove any type = 'plot-point'
                          g = newSVGNode("g", parent = parent, at = pos -1L, .children = x, attrs = c(pch = d[1, "pch"], type = "plot-point", class = "plot-point"))
                          TRUE
                     } else {
                        xmlAttrs(x[[1]]) = c(pch = d[1,"pch"], type = "plot-point", class = "plot-point")
                        FALSE
                     }
                   })

   num
}
