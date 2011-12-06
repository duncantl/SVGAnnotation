\name{getNodeElements}
\alias{getNodeElements}
\alias{getEdgeElements}
\alias{addGraphIds}
\alias{getEdgeInfo}
\alias{getEdgeInfo,graphNEL-method}
\title{Functions for working with Rgraphviz plots in SVG}
\description{
  These functions provide some support  for working with 
  graphs in SVG generated via Rgraphviz. 

  \code{addGraphIds} takes the graph layout (\code{\link[Rgraphviz]{Ragraph-class}})
  object and determines the SVG elements corresponding to the nodes and edges
  and places id and class attributes on each. This allows us to easily identify
  the elements in subsequent computations, both in R and JavaScript.

  A single node in the graph typically has two SVG elements associated with it:
  the circle and the text/label. We treat the circle as the node.
  The ids for the node circles are taken as the names from the graph layout object.
  The ids for the node labels are the same names but with "-label" appended.
 
  The id for an edge is slightly more complex. The general format is 
    "edge:tail-head", where tail and head are the names of the relevant nodes. 
   For undirected edges, the order is important.
   The head is the earlier node in the list of node names.
  (We haven't dealt with directed edges yet.)

  The edges have class "edge" and the nodes  "node" and the text of the node "nodeLabel"

  \code{getNodeElements} and \code{getEdgeElements} can be used
  to get the SVG elements in the document that correspond to the
  nodes and edges using the class attribute added by \code{addGraphIds}.
  These functions will not work correctly before \code{addGraphIds} has been called.

  \code{getEdgeInfo} is used by \code{addGraphIds} and by users annotating SVG documents in R
  to get the edge identifiers (i.e. the id attributes) in the correct form.
  Unlike the other functions, this does not work with the SVG document, but with the graph object directly.
  There are methods for a \code{graphNEL} object and a laid-out \code{Ragraph}

  These functions are not necessarily complete (!)  and will not
  necessarily work for different types of graph layouts.  The basic
  ideas however should extend readily to these other types but have
  not been implemented yet.  Currently, the functions will typically
  only work correctly when each edge corresponds to just one SVG
  element. This is the case for the twopi layout.

}
\usage{
addGraphIds(doc, graph,
            main = getNodeSet(doc,
                              "//s:svg/s:g/s:g",
                              c("s"= "http://www.w3.org/2000/svg"))[[1]])
getNodeElements(doc)
getEdgeElements(doc)
getEdgeInfo(graph, ...)
}
\arguments{
  \item{doc}{the parsed XML/SVG document}
  \item{graph}{the graph object. For \code{addGraphIds}, this should be an object of class \code{Ragraph}
     returned by a call to \code{agopen}.  This is the already laid-out graph.
     For \code{getEdgeInfo}, this can also be a graph description, e.g \code{graphNEL} object.
  }
 \item{main}{the XML element in the SVG document which contains (as children) the elements for
     the nodes and edges in our graph}
 \item{\dots}{extra parameters for methods to specialize}
}
\value{
  
  \code{getNodeElements} returns a list of \code{XMLInternalNode}
  objects corresponding to the nodes of the graph in the SVG display.
  Similarly, \code{getEdgeElements} returns a list of
  \code{XMLInternalNode} objects corresponding to the edges in the
  graph.

  \code{addGraphIds} returns a list with three elements
  \itemize{
    \item{edgeIds}{a character vector giving the values of the \code{id}
      attributes for the SVG elements corresponding to the graph's
      edges.}
    \item{edgeLabels}{a character vector giving the text labels for the edges.}
    \item{nodeIds}{a character vector giving the values of the \code{id}
        attributes for the SVG elements corresponding to the graph's nodes.}    
  }
  
  \code{getEdgeInfo} returns a list with as manay elements
  as there are edges. Each element is a character vector giving the
  "edge:src-dest" label identifying the connections..
  
}
\references{
 graph and Rgraphviz packages.
 BioConductor book "Bioinformatics and Computational Biology Solutions Using R and BioConductor".
 Gentleman, Carey, Huber, Irizarry and Dudoit (Editors). 2005.
}
\author{Duncan Temple Lang}
\seealso{
  \code{\link{svgPlot}}
}

\examples{
  # See the example in the tests/ directory of the package.
}
\keyword{hplot}
\keyword{iplot}


