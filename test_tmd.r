test_tmd <- function(code, im_opts="-density 300"){
  require(animation)
  require(knitr)
  require(png)
  require(grid)
  
  #compile tex
  knit2pdf(code, compiler = "xelatex", quiet = TRUE)

  #convert to png (need imagemagick)
  im.convert(paste0(sub("^([^.]*).*", "\\1", code), ".pdf")
               ,output = paste0(sub("^([^.]*).*", "\\1", code), ".png")
               ,extra.opts=im_opts
               ,cmd.fun = system)
    
  #read png
  img <- readPNG(paste0(sub("^([^.]*).*","\\1", code), '.png'))
    
  #return png file
  return(grid.raster(img)) 
}

