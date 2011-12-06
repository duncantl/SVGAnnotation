# This is a very limited type of path, i.e. a rectangle or a line.

makePolygonPath =
function(path)
{
  new("Polygon", path)
}

makePolylinePath =
function(path, checkRectangle = TRUE)
{
  ans = new("Polyline", path)
  if(checkRectangle) {
     m = matrix(unlist(ans), , 2, byrow = TRUE)
     bb = as(ans, "RectangularBoundingBox")
     if(all(m[,1] %in% bb[,1]) && all(m[,2] %in% bb[,2]))
         return(m)
  }
  ans
}

makeCirclePath =
function(p)
{
  i = c(1,3)
  x = mean(p[[2]][i])
  y = mean(c(p[[2]][c(1, 3, 5) + 1], p[[3]][c(1, 3, 5) + 1]))
  r = abs(diff(p[[2]][i]))/2
  new("Circle", c(x, y, r))
}


getGeneralPath =
  #
  # This reads arbitrary SVG paths and returns a list whose names identify the operations
  # and whose elements represent the values associated with that operation.
  # getGeneralPath("M 185.789062 261.632812 C 185.789062 265.234375 180.390625 265.234375 180.390625 261.632812 C 180.390625 258.035156 185.789062 258.035156 185.789062 261.632812 ")
  # yields a list with an M, a C and another C.
  # The elements have length 2, 6, 6.
function(data)
{
  if(is(data, "XMLInternalNode"))
     data = xmlGetAttr(data, "d")
  
  els = strsplit(data, "[MLZCQHVSQTAZmlcqhvsqtaz] ")[[1]]
  els = els[els != ""]
  ops = strsplit(gsub("[-0-9. ]", "", data), "")[[1]]

  i = !(ops %in% c("Z", "z"))

  ans = replicate(length(ops), numeric(), simplify = FALSE)

  ans[i] = mapply(function(x, op) {
#                        if(op == "Z")
#                          return(numeric())
                        con = textConnection(x)
                        on.exit(close(con))                     
                        scan(con, quiet = TRUE)
                      }, els, ops[i], SIMPLIFY = FALSE)
  names(ans) = ops
  ans
}

getPath =
function(data)
{
  #XXX handle CQHVhvSQTAZ
  # http://www.w3schools.com/svg/svg_path.asp
  # See getGeneralPath()
   els = strsplit(data, "[MLZ]")[[1]]

if(TRUE) {   
   con = textConnection(els)
   on.exit(close(con))
   els = scan(con, quiet = TRUE)
 } else
   els = as.numeric(els)

   #XXX deal with Z
   ops = strsplit(gsub("[0-9.Z]", "", data), "[[:space:]]+")[[1]]

   new("SVGPath",  matrix(els, ,2, byrow = TRUE, dimnames = list(NULL,  c("x", "y"))))
}



getPathObject =
  #
  # idea here is to see if we can interpret the path as being a line,
  # a circle, a rectangle, a polyline
function(data)
{
  con = textConnection(gsub(" ([C])", "\\\n\\1", str))
  on.exit(close(con))
  els = readLines(con)
  type = substring(els, 1, 1)
  steps = lapply(strsplit(gsub("(^ +| +$)", "", substring(els, 2)), " "), as.numeric)
  names(steps) = type
  steps
}

isCircle =
  #
  # is path a circle
  #  
function(path, start = c(0, 0))
{
  i = seq(1, by = 2, length = 4)
  x = c(start, path)[i]
  y = c(start, path)[i + 1]  
  x[1] == x[2] && x[3] == x[4] && y[1] == y[4] && y[2] == y[3] 
}


isCirclePath =
  #
  # Requires a general path. See getGeneralPath.
  #
function(path)
{
  length(path) == 3 && all(names(path) == c("M", "C", "C")) &&
    all(path[[2]][c(1, 6)] == path[[3]][5:6])
}


isPolylinePath =
function(path)
{
  names(path)[1] == "M" && all(names(path)[-1] == "L")
}  

isPolygonPath =
function(path)
{
  n = length(path)
  if(!all(names(path)[c(1, n-1, n)] == c("M", "Z", "M")))
    return(FALSE)

  
  if(n > 7) # not a rectangle. 4 L elements + M Z M.
    return(TRUE)

  #XXX need to check if this is a rectangle or just 4 lines.
  # So need to look at the path itself.
  # Could also be 3 with a Z to close the rectangle.  
   
  FALSE
}

isLinePath =
function(path)
{
  (length(path) == 2 || length(path) == 3) && c(names(path)[1] == "M" && names(path)[-1] == "L")
}

isHorizontalLine =
function(path)
  path[[1]][2] == path[[2]][2]

isVerticalLine =
function(path)
  path[[1]][1] == path[[2]][1]

makeLinePath =
function(path)
{
  class = if(isHorizontalLine(path))
             "HorizontalLine"
          else if(isVerticalLine(path))
             "VerticalLine"
          else
             "Line"
  new(class, as.numeric(unlist(path)))
}


isRectPath =
function(m)
{
  nrow(m) == 5 && length(unique(m[,1])) == 2 && length(unique(m[,2])) == 2
}
