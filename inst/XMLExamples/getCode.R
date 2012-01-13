library(XML)
ff = list.files(pattern = "xml$")
txt = lapply(ff, xmlSource, parse = FALSE)

ntxt = mapply(function(origin, code)
               c(sprintf("##########################\n### Code from %s", origin), code, "\n"),
             ff, txt)
            

mapply(function(out, code)
        cat(code, file = out, sep = "\n"),
       sprintf("RCode/%s", gsub("\\.xml", ".R", ff)),
       ntxt)

tmp = lapply(sprintf("RCode/%s", gsub("\\.xml", ".R", ff)),
               parse)

cat(unlist(ntxt), file = "RCode/SVGAnnotationPaper.R", sep = "\n")
invisible(parse("RCode/SVGAnnotationPaper.R"))

               



       
