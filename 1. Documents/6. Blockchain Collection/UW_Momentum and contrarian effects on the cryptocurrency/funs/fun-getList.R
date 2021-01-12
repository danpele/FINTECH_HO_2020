getList <- function(dir = "data/crypto/singleDates"){
  files <- paste0(dir, list.files(dir))
  data.list <- sapply(files, 
                      function(x) readRDS(x), simplify = F, USE.NAMES = T)
  list.names <- substr(names(data.list), nchar(dir) + 1, 10000)
  list.names <- gsub(".rds", "", list.names)
  names(data.list) <- list.names
  return(data.list)
}

