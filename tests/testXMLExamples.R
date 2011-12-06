src = list.files("../inst/XMLExamples", pattern = "(xml|Rdb)$", full.names = TRUE)

library(XML)
res = sapply(src,
        function(f) {
           cat("running", f, "\n")
           xmlSource(f, verbose = TRUE)
        })
