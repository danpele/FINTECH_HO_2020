getCryptoPricesForGivenDate <- function(thisDate, 
                                        saveData = T,
                                        outputPath = "data/crypto/singleDates/") {
  
  cat("scraping data for", 
      thisDate %>% as.Date(origin = "1970-01-01") %>% as.character(), 
      "... ")
  
  thisDate <- thisDate %>% as.Date(origin = "1970-01-01")
  theurl <- RCurl::getURL(paste0("https://coinmarketcap.com/historical/",
                          thisDate %>% format("%Y%m%d"),
                          "/"))
  table <- XML::readHTMLTable(theurl, stringsAsFactors = FALSE)
  table <- rlist::list.clean(table, fun = is.null, recursive = FALSE)
  table <- table[[3]]
  
  dt <- 
    as_tibble(table[, c("Name", "Symbol", "Market Cap",
                        "Price", "Circulating Supply", "Volume (24h)")]) %>%
    mutate(Name   = gsub(".*\n", "", Name, perl = T),
           Ticker = gsub("\n.*", "", Name, perl = T)) %>%
    rename(MarketCap = `Market Cap`,
           Supply = `Circulating Supply`,
           Volume = `Volume (24h)`) %>%
    mutate(MarketCap = MarketCap %>% gsub("\\$", "", .) %>% gsub(",", "", .),
           Price     = Price     %>% gsub("\\$", "", .) %>% gsub(",", "", .),
           Supply    = Supply    %>% gsub("\\$", "", .) %>% gsub(",", "", .) %>% 
             gsub("\\n*", "", .) %>% gsub("\\*", "", .),
           Volume    = Volume    %>% gsub("\\$", "", .) %>% gsub(",", "", .)) %>%
    mutate(MarketCap = ifelse(MarketCap == "?", "0", MarketCap),
           Price     = ifelse(Price     == "?", "0", Price),
           Supply    = ifelse(Supply    == "?", "0", Supply),
           Volume    = ifelse(Volume    == "Low Vol", "0", Volume)) %>%
    #filter(!(MarketCap == "?")) %>%
    #filter(!(Price == "?")) %>%
    #filter(!(Supply == "?")) %>%
    # filter(!(Volume == "Low Vol")) %>%
    mutate(MarketCap = MarketCap %>% as.double(),
           Price = Price %>% as.double(),
           Volume = Volume %>% as.double(),
           Supply = Supply %>% as.double(),
           Date = thisDate %>% as.Date(),
           # test = (Name == Ticker)
    ) %>%
    select(Date, Name, Symbol, Price, MarketCap, Supply, Volume)
  
  if (saveData) {
    saveRDS(dt, paste0(outputPath, 
                       thisDate %>% format("%Y%m%d"), ".rds"))
  }
  
  cat("done!\n")
  
  return(dt)
  
}
