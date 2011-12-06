#
# 4 points
#  1 going along a 45 degree line
#  one going 
#
#

p1 = matrix(c( 2, 2, 2,
               3, 3, 3,
               4, 4, 4,
               5, 5, 5, 
               6, 6, 6), 5, , byrow = TRUE)

p2 = matrix(c( 12, 2, 1,
               10, 3, 1.2,
                8, 4, 1.4,
                6, 5, 1.5, 
                4, 6, 3), 5, , byrow = TRUE)


p3 = cbind(seq(4, 12, length = 5),
           rep(6, 5),
           c(1.5, 3, 5, 3, 1.5))

D = data.frame(rbind(p1, p2, p3))

names(D) = c("x", "y", "radius")
D$time = rep(1:5, 3)
D$id = rep(1:3, each = 5)
D = D[order(D$time),]


if(FALSE) {
plot(0, xlim = range(D$x), ylim = range(D$y))
by(D, D$id, function(x){
             col = c("red", "green", "blue")[x[,"id"]]
             points(x[,1], x[,2], col = col)
             text(x[,1] + .1, x[,2] + .1, 1:nrow(x), col = col)             
             lines(x[,1:2], col = col[1])
           })
}

library(SVGAnnotation)

xlim = range(D$x) + c(-1, 1)
ylim = range(D$y) + c(-1, 1)

pp = svgPlot(plot(y ~ x, subset(D, time == 1), pch = 21,
                   col = c("red", "green", "blue"),
                   bg = c("red", "green", "blue")
                    ))
   #                 xlim = xlim, ylim = ylim)

#animate(pp, subset(D, time != 1), D[D$time != 1, "time"], xlim = xlim, ylim = ylim)
#animate(pp, subset(D, time != 1), "time", xlim = xlim, ylim = ylim)

#animate(pp, D, "time", dropFirst = TRUE, labels = paste("Stage", 1:5))

 # Compute the radius 
radii = by(D[D$time != 1,], D[D$time != 1, "id"], function(x) x$radius)


#################
animateRadius =
function(node, radii, durations, begin)
{
  radii = radii * as.numeric(xmlGetAttr(node, "r"))
  mapply(function(r, d, begin) {
           newXMLNode("animate", attrs = c(attributeName = "r", fill = "freeze", attributeType = "XML",
                                   to = r, dur = d, begin = begin), parent = node)
         }, radii, durations, begin)
}

circles = animate(pp, D, "time", dropFirst = 1, colors = character()) # xlim = xlim, ylim = ylim,

mapply(animateRadius,
        circles$circles, radii, MoreArgs = list("2s", paste(seq(0, 8, by = 2), "s", sep = "")))
       

saveXML(pp, "/tmp/gm.svg")


################

pp = svgPlot(plot(y ~ x, subset(D, time == 1), pch = 21,
                   col = c("red", "green", "blue"),
                   bg = c("red", "green", "blue")
                   )) #  xlim = xlim, ylim = ylim

region  = getPlotRegionNodes(pp)

animRadius =
  #
  # This version gets called numSteps times
  #  for each circle node, i.e. it is called for each stage/step
  # in the animation for each node. So lots!
  #
  # Each time it is called details is a list containing
  #   index - the index of the observation, 1, 2, ... n
  #   step - index of step/stage
  #   dur - the duration of this stage
  #   begin - the start time for this stage
  #   id - the id for this step.
  #
  # radius is an additional argument we pass to animate
  #
function(node, details, radius)
{
  r = radius[[details$index]] * as.numeric(xmlGetAttr(node, "r"))
  newXMLNode("animate", attrs = c(attributeName = "r",
                                   fill = "freeze",
                                   attributeType = "XML",
                                   to = r[details$step], dur = details$dur, begin = details$begin), parent = node)
}  

if(FALSE) {
   # This versions passes additional parameters via ...
info = animate(pp, D, "time", dropFirst = 1, xlim = xlim, ylim = ylim, colors = character(),
                hook = animRadius, radius = radii)
} else
   # This version uses .args to avoid name collisions.
info = animate(pp, D, "time", dropFirst = 1, colors = character(),
                hook = animRadius, .args = list(radii), dur = 5) #, xlim = xlim, ylim = ylim

saveXML(pp, "/tmp/gm1.svg")


##########################################
if(TRUE) {
vb = as.numeric(dim(pp))

box = getBoundingBox(region[[1]])
pos = as.numeric(colMeans(box))

if(FALSE) {
BGText = c( "Hi there", "Good Bye");
jsCode = 'function changeText(i) {
  var obj = document.getElementById("BG");

  obj.firstChild.nodeValue = BGText[i-1];
}'
addECMAScript()
}


  # Put the text elements into their own group and put this just after the rect
  # element so that it is drawn first and so is "underneath" the other elements,
  # i.e. the circles are drawn on top of the text.
g = xmlRoot(pp)[["g"]]
g = newXMLNode("g", parent = g, attrs = c(id = 'backgroundText'), at = 1)

labels = paste("Step", 1:length(info$details$dur))
print(length(labels))
els = mapply(function(when, val, dur, to, freeze) {
               text = newXMLNode("text", val, parent = g,
                                    attrs = c(x = pos[1], y = pos[2], 'font-size' = 60, fill = 'lightgray', 'text-anchor' = "middle",
                                               visibility = if(to == "hidden") 'visible' else 'hidden', id="BG"))
  
               newXMLNode("set", attrs = c(attributeName = 'visibility', 
                                           attributeType = "XML",
                                           to = to,
                                           if(to == 'visible') c(begin = when, dur = dur) else c(begin = dur, dur = "0s"),
                                           if(freeze) c(fill = "freeze")),
                           parent = text)
               text
       }, info$details$start, labels, info$details$dur,
          c("hidden", rep('visible', length(labels)-1)),
          c(TRUE, rep(FALSE, length(labels) - 2), TRUE)
          )
#xmlAttrs(els[[1]]) <- c(visibility = 'visible')


saveXML(pp, "/tmp/gm2.svg")
}
