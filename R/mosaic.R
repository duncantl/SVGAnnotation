if(FALSE) {
  a = matrix(as.integer(rnorm(12, 100, 10)), 3, 4, dimnames = list(c("A", "B", "C"), c("W", "X", "Y", "Z")))
  names(dimnames(a)) = c("Var 1", "Var 2")
  saveXML(annotateMosaic(svgPlot(o <- mosaic(a)), o), "/tmp/simpleMosaic.svg")

  saveXML(annotateMosaic(svgPlot(o <- mosaic(a)), o, cellTips = function(x) x$.count), "/tmp/mySimpleMosaic.svg")

  saveXML(annotateMosaic(svgPlot( m <- mosaic(UCBAdmissions)), m), "/tmp/ucb.svg")

  saveXML(annotateMosaic(svgPlot(z <- mosaic(Survived ~ ., data = Titanic)), z), "/tmp/titanic.svg")


  xx = data.frame(a = sample(1:3, 1000, TRUE), b = sample(c("A", "B", "C", "D"), 1000, TRUE),
                  c = sample(c("X", "Y"), 1000, TRUE), d = sample(c("Yes", "No"), 1000, TRUE))

  saveXML(annotateMosaic(svgPlot(m <- mosaic(with(xx, table(a, b, c, d)))), m), "/tmp/multiMosaic.svg")

    # With zeros
  v = matrix(c(2, 0, 3,
               4, 1, 2,
               0, 1, 4), 3, 3, byrow = TRUE,
             dimnames = list(c("A", "B", "C"), c("x", "y", "z")))

  saveXML(annotateMosaic(svgPlot(m <- mosaic(v)), m), "/tmp/zerosMosaic.svg")
  
   # Example of 5 variables.
  xx = data.frame(a = sample(1:3, 100, TRUE), b = sample(c("A", "B", "C", "D"), 100, TRUE),
                  c = sample(c("X", "Y"), 100, TRUE), d = sample(c("Yes", "No"), 100, TRUE),
                  e = sample(c("red", 'green', 'blue'), 100, TRUE))
  saveXML(annotateMosaic(svgPlot(m <- mosaic(with(xx, table(a, b, c, d, e)))), m), "/tmp/fiveVarMosaic.svg")
  
}

annotateMosaic =
  #
  # zdoc = svgPlot(z <- mosaic(Survived ~ ., data = Titanic))
  # sex = svgPlot(sex.table <- mosaic(~ ExtramaritalSex + PremaritalSex | MaritalStatus + Gender, data = PreSex))
  # sex = svgPlot(mosaic(~ ExtramaritalSex + PremaritalSex | Gender, data = PreSex))
  #
  # x =  mosaic(~ gear + carb, data = mtcars)
  # x =  mosaic(~ gear + carb | am, data = mtcars)
  # x =  mosaic(~ gear + carb | cyl, data = mtcars)
  #
  #
  # 
  #  saveXML(annotateMosaic(svgPlot( m <- mosaic(UCBAdmissions)), m), "/tmp/ucb.svg")
  #
  #  Fix the levels.
  #  Add tooltips to labels.
  #
  #  Handle the case where there are 0 cells that span multiple columns, e.g. titanic data and
  #  the no children in the crew leading 
  #
  #
  #
  #
function(doc, table, cellTips = NULL, 
         axisLabelTips = c(names(attr(table, "col.vars")), names(attr(table, "row.vars"))),
         categoryTips = getMosaicLabels(table),
         shade = FALSE, addCSS = TRUE)
{
  if(is.character(doc))
    doc = xmlParse(doc)
  
  g = xmlRoot(doc)[["g"]]
    # discard the <rect> node from our list of children, i.e. stays in the SVG document.
  kids = xmlChildren(g)[-1]

  groupZeroCellNodes(kids) # Want to go up to the where the axes labels are. But
                           # don't know where this is without knowing the zeros!
                           # XX Make certain we don't find zero nodes after the axes labels.
                      #  xmlSApply(xmlRoot(doc)[["g"]], function(x) all(xmlSApply(x, SVGAnnotation:::isTextNode)))
  kids = xmlChildren(g)[-1]  
  
    # If there is a shading, then we have a legend.
  if(shade) {
     setTypeAttr(kids[[1]], "mosaic-key")
     setTypeAttr(kids[[2]], "mosaic-legend-labels")
     kids = kids[ - c(1:2) ]
  }

    # Now we work on the axis labels and put their labels on.
  numCells = prod(dim(table))

      # identify the  elements that correspond to 0s in the table.
  counts = as.integer(t(table))
  cellNodes = seq(length = numCells)
#  obs = rep(cellNodes, c(1, 2)[(counts  == 0) + 1L])

  i = xmlSApply(g, function(x) xmlSize(x) > 0 && all(xmlSApply(x, SVGAnnotation:::isTextNode)))
  
  if(length(axisLabelTips))  {
      labels =  g[[ which(i)[1] ]]
      mapply(function(node, id) {
               xmlAttrs(node) = c(variable = id, type = "axis-label", class = "axis-label", text = id)
               addToolTips(node, id)
             },
               xmlChildren(labels), axisLabelTips)
   }


  if(length(categoryTips)) {
     levelLabelNodes =  g[ seq(which(i)[1] + 1, xmlSize(g)) ]
     addMosaicTickLabels(table, levelLabelNodes, categoryTips)
  }
  
    # now we work on the actual data cells, e.g. the rectangles
    # and put the count on them along with the values of the different
    # categorical variables.
    # We have to recognize empty groups that are collapsed to a single "zero node".
  levels = mosaicLevels(table)

  if(is.function(cellTips)) {
    opts = do.call("expand.grid", dimnames(table)[rev(names(dimnames(table)))])
    opts$.count = as.integer(t(table))
    cellTips = by(opts, 1:nrow(opts), cellTips) # keep as a data frame not a matrix.
  } else if(is.null(cellTips))
    cellTips = paste(1:numCells, levels, as.integer(t(table)), sep = ": ")


  if(length(cellTips))
     mapply(function(node, num, levels, tip, i) {
               xmlAttrs(node) = c(count = num, levels = levels)
               addToolTips(node, tip)
            }, kids[ 1:numCells ], as.integer(t(table)), levels, cellTips, 1:numCells)

  if(is.character(addCSS))
     addCSS(doc, addCSS)
  else if(addCSS)
     addCSS(doc)

  invisible(doc)
}

getCategoryLabelNodes.mosaic =
function(doc, table = NULL, numCells = prod(dim(table)), groupZeros = NA)
{
  update = FALSE
  ans = getNodeSet(doc, "//x:g[@type='mosaicCategoryLabel']", "x")

  if(length(ans) == 0) {
    g = xmlRoot(doc)[["g"]]
    if(is.na(groupZeros) || groupZeros)
      groupZeroCellNodes(xmlChildren(g)[-1])
    i = xmlSApply(g, function(x) xmlSize(x) > 0 && all(xmlSApply(x, SVGAnnotation:::isTextNode)))  
    ans = xmlChildren(g)[ seq(which(i)[1] + 1, xmlSize(g)) ]
    update = TRUE
  }

  if(!is.null(table)) {
    names(ans) = getMosaicLabels(table)
    if(update)
      mapply(function(node, label)
             xmlAttrs(node) = c(label = label, type = "mosaicCategoryLabel", class = "mosaicCategoryLabel"),
             ans, names(ans))    
  } else if(update)
     sapply(ans, setTypeAttr, "mosaicCategoryLabel")
  
  ans
}

getAxesLabelNodes.mosaic =
function(doc, table = NULL, groupZeros = NA)
{
  update = FALSE
  ans = getNodeSet(doc, "//x:*[@type='axisLabel']", "x")
  
  if(length(ans) == 0) {
    
    g = xmlRoot(doc)[["g"]]
    if(is.na(groupZeros) || groupZeros)
      groupZeroCellNodes(xmlChildren(g)[-1])  

    i = xmlSApply(g, function(x) xmlSize(x) > 0 && all(xmlSApply(x, isTextNode)))

    w = which(i)[1]
    if(w == 2)    # For the case of the examples which add a text node at the beginning of the plot!!!!
      w = which(i)[2]
    
    labels =  g[[ w ]]  # first of these is a <g> that contains the labels

    ans = xmlChildren(labels)
    update = TRUE
  }
  
  if(!is.null(table)) {
    names(ans) = names(dimnames(table))
    if(update)
      mapply(function(node, label)
                xmlAttrs(node) = c(label = label, type = "axisLabel", class = "axisLabel"),
             ans, names(ans))    
  } else if(update)
     sapply(ans, setTypeAttr, "axisLabel")
  
  ans
}

addMosaicTickLabels =
  #
  #  compute and optionally add information about the labels
  #  of the categories on the different axes of the mosaic plot.
  #  These are organized by row and within row by columns.
  #
  #
  # Basically, the first label is the last element of the dimnames(table)[[1]]
  # Then we do the labels on the lower horizontal axis, but only if there is a fourth
  # variable.
  # Next come the labels on the right vertical axis corresponding to the 
function(table, nodes = list(), phrases = getMosaicLabels(table), add = length(nodes) > 0)
{
  row = lapply(attr(table, "row.vars"), rev)
  col = attr(table, "col.vars")

  numVars = length(dim(table))

  if(add)
      mapply(function(node, id, i) {
                xmlAttrs(node) = c(text = id)
                addToolTips(node, paste(i, ")", id))
             }, nodes, phrases, seq(along = nodes))

  phrases
}


getMosaicLabels =
function(table)
{
  numVars = length(dim(table))
  ans = character()
  dn = dimnames(table)

          # The labels on the left and right are in reverse order within those vectors.
  if(numVars == 2) {
          # the vertical labels are first and then the horizontal ones on the top.
     for(i in rev(dn[[1]]))
        ans = c(rev(dn[[1]]), dn[[2]])
  } else if(numVars == 3) {
        # We have labels on the left, right and top.
        # On the right, these are repeated for each row.
     ctr = 1
     for(i in rev(dn[[1]])) {
        ans = c(ans, i, if(ctr == length(dn[[1]])) dn[[2]], rev(dn[[3]]))
        ctr = ctr + 1
     }
  } else if(numVars == 4) {
     ctr = 1
        # for the first row, we have the label on the left,
        # the labels on the bottom repeated for as many columns as there are
        # which corresponds to the number of labels on the  top.
        # However, the last column is different. We have the first label, then
        # the labels on the right and then remaining labels on the bottom of that column.
     for(i in rev(dn[[1]])) {
        ans = c(ans, i, if(ctr == 1) rep(dn[[4]], length(dn[[2]]) - 1)
                        else if(ctr == length(dn[[1]])) dn[[2]],
                     if(ctr == 1) c(rev(dn[[3]])[1], dn[[4]], rev(dn[[3]])[-1]) else rev(dn[[3]]))
        ctr = ctr + 1
     }
  } else {
     seq(length = prod(dn))
  }

  ans
}



mosaicLevels =
function(table, vars =  rev(names(dimnames(table)))) # c(c(attr(table, "col.vars")), c(attr(table, "row.vars"))))
{
  combs = do.call("expand.grid", dimnames(table)[ vars ])

  levels = mapply(function(x, col) paste(x, col, sep = "="),
                   vars, combs)
  levels = apply(levels, 1, paste, collapse = ", ")
  levels
}


groupZeroCellNodes =
function(nodes)
{
   if(length(nodes) == 0)
     return(FALSE)

   disp = getNodeSet(as(nodes[[1]], "XMLInternalDocument"), "//r:display[@groupZeroNodes='true']", c(r = "http://www.r-project.org"))
   if(length(disp))
     return(FALSE)
  
   i = sapply(nodes, isZeroCircleNode)
   if(any(i)) {
     i = which(i)
     mapply(groupZeroCellNode, nodes[i], nodes[i+1])
   }
   disp = getNodeSet(as(nodes[[1]], "XMLInternalDocument"), "//r:display", c(r = "http://www.r-project.org"))
   r = disp[[1]]
   xmlAttrs(r) = c(groupZeroNodes = "true")
   TRUE
}

isZeroCircleNode =
function(node)
{
  if(!(xmlName(node) == "g" && xmlSize(node) == 3 && names(node) == "path"))
    return(FALSE)
  shapes = xmlApply(node, getShape)
  all(sapply(shapes, class) == c("HorizontalLine", "HorizontalLine", "Circle"))
}

groupZeroCellNode =
function(circ, line, parent = xmlParent(circ))
{
    # No need to do this regrouping - already in a <g>
  if(TRUE) {
    setTypeAttr(circ, "mosaic-zero-cell")
    return(circ)
  }

  #IGNORE THIS.
  i = XML:::indexOfNode(circ)
  node = newXMLNode("g", attrs = c(type = "mosaic-zero-cell", class = "mosaic-zero-cell"), parent = parent, at = i - 1)
  removeNodes(list(circ, line))
  addChildren(node, c(circ, line))
  setSVGNs(node)
  node
}




