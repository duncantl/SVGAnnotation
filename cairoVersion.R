determineCairoVersion =
function(dir = R.home(), id = sprintf("R_X11%s", .Platform$dynlib.ext))
{
  f = list.files(dir, pattern = id, full.names = TRUE, recursive = TRUE)
  txt = system(sprintf("strings %s", f[1]), intern = TRUE)
  tt = grep("cairo-([0-9.]+)/", txt, value = TRUE)
  getCairoVersion(tt)
}

getCairoVersion =
function(tt)
{
  if(!length(tt))
    return("NA")

  vals = unique(gsub(".*/cairo-([0-9.]+)/.*", "\\1", tt))
  if(length(vals) > 1)
     warning("Multiple values")
   vals[1]
}
