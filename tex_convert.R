tex_convert <- function(code, im_opts="-density 300"){
  require(animation)
  require(knitr)
  
  #compile tex to pdf
  knit2pdf(code, compiler = "xelatex", quiet = TRUE)

  #convert pdf to png (need imagemagick)
  im.convert(paste0(sub("^([^.]*).*", "\\1", code), ".pdf")
            ,output = paste0(sub("^([^.]*).*", "\\1", code), ".png")
            ,extra.opts=im_opts
            ,cmd.fun = system)
}

