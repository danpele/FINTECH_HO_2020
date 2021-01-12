getDataFromStooq <- function(ticker     = "^spx",
                             start.date = "2013-01-01",
                             stop.date  = NULL,
                             outputPath = "data/regular/",
                             saveData   = T) {
  
  if (is.null(stop.date)) {
    stop.date <- Sys.Date() - 1
  }
  
  thisURL <- paste0("https://stooq.com/q/d/l/?s=",
                    ticker,
                    "&d1=",
                    start.date %>% gsub("-", "", .),
                    "&d2=",
                    stop.date %>% gsub("-", "", .),
                    "&i=d")
  data <- read_csv(thisURL)
  
  saveRDS(data, paste0(outputPath, 
                       ticker %>% gsub("\\^", "", .), 
                       ".rds") )
  
  return(data)
  
}


