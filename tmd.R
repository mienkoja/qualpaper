tmd <- function(code
                ,im_opts="-density 300"
                ,pdf_opts="scale=0.8,pages={1}"
                ,k2p=FALSE){
  require(animation)
  require(knitr)
  require(png)
  require(grid)
  
  #compile tex
  knit2pdf(code, compiler = "xelatex", quiet = TRUE)

  if(k2p==FALSE){
    #convert to png (need imagemagick)
    im.convert(paste0(sub("^([^.]*).*", "\\1", code), ".pdf")
               ,output = paste0(sub("^([^.]*).*", "\\1", code), ".png")
               ,extra.opts=im_opts
               ,cmd.fun = system)
    
    #read png
    img <- readPNG(paste0(sub("^([^.]*).*","\\1", code), '.png'))
    
    #return png file
    return(grid.raster(img)) 
  } else{
    #return pdf
    return(cat(paste0('\\includepdf['
                      ,pdf_opts
                      ,']{'
                      ,sub("^([^.]*).*","\\1", code)
                      ,'.pdf}'
    )))
  }
}

