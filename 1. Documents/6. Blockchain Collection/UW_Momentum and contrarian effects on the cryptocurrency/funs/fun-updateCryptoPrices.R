updateCryptoPrices <- function(start.date = NULL,
                               stop.date = NULL,
                               outputPath = "data/crypto/singleDates/",
                               longTibbleFile = "data_crypto.rds",
                               updateLongTibble = T,
                               pauseLength = 15) {
  
  
  
  if (is.null(start.date) & is.null(stop.date)) {
    
    all.dates <- seq(from = as.Date("2019-09-07"),
                     to   = Sys.Date() - 1, 
                     by   = 1) %>% as.Date(origin = "1970-01-01")
    
    dates.in.folder <- dir(outputPath) %>% 
      substr(1, 8) %>% 
      as.Date(format = "%Y%m%d")
    
    dates.to.update <- setdiff(all.dates, dates.in.folder) %>% 
      as.Date(origin = "1970-01-01")
  
    for (i in dates.to.update) {
      getCryptoPricesForGivenDate(thisDate = i)
      Sys.sleep(15)
    }
  } else {
    
    dates.to.update <- seq(from = start.date %>% as.Date(),
                           to   = stop.date %>% as.Date(),
                           by   = 1) %>% as.Date(origin = "1970-01-01")
    
    for (i in dates.to.update) {
      getCryptoPricesForGivenDate(thisDate = i)
      Sys.sleep(pauseLength)
    }
    
  }

  if (updateLongTibble) {
    
    # tb <- dir(outputPath) %>%
    #   gsub("-", "", .) %>%
    #   paste0(outputPath, .) %>%
    #   map(read_rds) %>%    
    #   reduce(bind_rows)
    
    tb <- dir(outputPath) %>%
      gsub("-", "", .) %>%
      paste0(outputPath, .) %>%
      map(read_rds) %>%    
      data.table::rbindlist(., use.names = TRUE, fill = TRUE) %>%
      as_tibble()
    
    tb2 <- tb %>%
      arrange(Date, Name, MarketCap %>% desc()) %>%
      group_by(Date, Name) %>%
      filter(row_number() == 1) %>%
      ungroup() %>%
      arrange(Date, MarketCap %>% desc())
    
    saveRDS(tb2, file = paste0("data/crypto/", longTibbleFile))
  }
  
    
}
  

