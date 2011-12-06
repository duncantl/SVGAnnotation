if(require(Rgraphviz)) {
xpkgs = c("R2GoogleMaps", "XML", "RCurl", "SSOAP", "XMLRPC", "RExcelXML", "ROOXML", "Sxslt", "RJSONIO",
          "SVGAnnotation", "digest", "RAmazonS3", "XMLSchema", "Rcompression", "CodeDepends", "FlashMXML",
          "RCSS", "Rflickr", "RGoogleDocs", "RHTMLForms", "RKML", "RUbigraph", "SASXML", "Zillow",
          "RAutoGenRunTime", "RGraphicsDevice")

dep = available.packages(contrib.url('http://www.omegahat.org/R', type = "source"))
i = xpkgs %in% rownames(dep)
g = utils:::.make_dependency_list(xpkgs[i], dep)
g$ROOXML = c("XML", "Rcompression", "methods")
g$digest = character()
g$RAutoGenRunTime = character()

xpkgs = c(xpkgs, setdiff(unlist(g), xpkgs))

setdiff(xpkgs, names(g))
setdiff(names(g), xpkgs)

edges = lapply(g, function(x) list(edges = match(x, xpkgs)))

o = setdiff(xpkgs, names(edges))
edges[o] = replicate(length(o), list(edges = integer()), simplify = FALSE)


library(Rgraphviz)
library(SVGAnnotation)

gr = new("graphNEL", nodes = xpkgs, edgeL = edges, "directed")
l = layoutGraph(gr)
renderGraph(l)

doc = svgPlot(renderGraph(l))

ll = agopen(gr, layoutType = "neato", name = "foo")
plot(ll)

}
