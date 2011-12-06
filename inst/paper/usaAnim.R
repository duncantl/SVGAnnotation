library(maps)
m = map('state', plot = FALSE)

f = 'usa.svg'
svgPlot(f, map('state'))

doc = xmlParse(f)
n = getNodeSet(doc, "/x:svg/x:g//x:path", "x")[[4]]
  #See electionHistory to get state.props
cols = sapply(state.props, function(x) x["california", 2, drop = TRUE])
delay = "1s"
sapply(seq(along = cols),
       function(i, parent) {
          p = cols[i]
          col = sprintf("rgb(%f%%,0%%,%f%%)", (1-p)*100, p*100)
          newXMLNode("set",
                      attrs = c(attributeName = "fill", attributeType = "CSS",
                                        to = col,
                                        fill = "freeze",
                                        begin = sprintf("%ds", i),
                                        dur = delay),
                      parent = parent)
        }, n)

# What's a swing state? One this is close to  50% or one with
# high variability and that has gone back and forth between the parties.
saveXML(doc, f)
