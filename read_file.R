read_file <- function(filename, silent = TRUE, sql_file = TRUE) {
  if (sql_file == TRUE) {
    t <- readLines(filename, warn = !silent)
    t <- t[!grepl(pattern = "^\\s*--", x = t)] # remove full-line comments
    t <- sub(pattern = "--.*", replacement="", x = t) # remove midline comments
    t <- paste(t, collapse = " ")
  }
  else {
    t <- readLines(filename, warn = !silent)
    t <- paste(t, collapse = " ")
  }
  return(t)
}

