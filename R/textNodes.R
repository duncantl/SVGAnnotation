
asTextNode =
function(node, text, replace = TRUE, addChildren = FALSE, ..., .attrs = list(...),
          vertical = NA)
{
  rect = getBoundingBox(node)
  pos = as.numeric(colMeans(rect))
  if(is.na(vertical))
    vertical = isVerticalText(node)

  xtra = c(x = pos[1], y = pos[2], "text-anchor" = "middle", style = xmlGetAttr(node, "style"))
  if(!is.na(vertical) && vertical)
     xtra["rotate"] = 90
  ids = c("x", "y", "text-anchor", "style")
  i = match(names(xtra), names(.attrs))
  if(any(is.na(i)))
     .attrs[ids[ is.na(i)] ] = xtra[ is.na(i) ]
  
  newNode = newXMLNode("text", text, attrs = .attrs)
  if(replace) {
    if(addChildren && xmlSize(node))
       addChildren(newNode, kids = xmlChildren(node))
    replaceNodes(node, newNode)

    setSVGNs(newNode)
  }

  newNode
}

isVerticalText =
  #
  #  Currently assumes is called with <g><use/>+</g>
  #
function(node)
{
  pos = xmlSApply(node, function(x) xmlAttrs(x)[c("x", "y")])

  all(pos[1,] == pos[1,1])
}
