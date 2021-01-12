createWeights <- function(rebDates     = rebalancing_dates,
                          cdata        = crypto,
                          rankType     = NA,
                          strategyType = NA,
                          includePct   = 0.05) {
  
  # verification of rankType provided
  stopifnot(rankType %in% c("MC", "1D", "1W", "1M"))
  
  cdata <- 
    cdata %>% 
    filter(rankMC <= 100)

  cdataAgg <-
    cdata %>%
    group_by(Date) %>%
    summarize(mcNum = max(rankMC)) %>%
    ungroup() %>%
    mutate(includeNum = as.integer(mcNum * includePct)) 
  
  d <-
    cdata %>%
    left_join(cdataAgg %>% select(Date, includeNum))
  
  if (rankType == "1D" & strategyType == "mom") {
    d <- 
      d %>% 
      arrange(Date, desc(ret1D)) %>%
      group_by(Date) %>%
      mutate(rank1D = row_number()) %>%
      ungroup() %>%
      filter(rank1D <= includeNum) %>%
      filter(Date %in% rebDates)
  }
  
  if (rankType == "1D" & strategyType == "con") {
    d <- 
      d %>% 
      arrange(Date, ret1D) %>%
      group_by(Date) %>%
      mutate(rank1D = row_number()) %>%
      ungroup() %>%
      filter(rank1D <= includeNum) %>%
      filter(Date %in% rebDates)
  }
  
  
  if (rankType == "1W" & strategyType == "mom") {
    d <- 
      d %>% 
      arrange(Date, desc(ret1W)) %>%
      group_by(Date) %>%
      mutate(rank1W = row_number()) %>%
      ungroup() %>%
      filter(rank1W <= includeNum) %>%
      filter(Date %in% rebDates)
  }
  
  if (rankType == "1W" & strategyType == "con") {
    d <- 
      d %>% 
      arrange(Date, ret1W) %>%
      group_by(Date) %>%
      mutate(rank1W = row_number()) %>%
      ungroup() %>%
      filter(rank1W <= includeNum) %>%
      filter(Date %in% rebDates)
  }
    
  if (rankType == "1M" & strategyType == "mom") {
    d <- 
      d %>% 
      arrange(Date, desc(ret1M)) %>%
      group_by(Date) %>%
      mutate(rank1M = row_number()) %>%
      ungroup() %>%
      filter(rank1M <= includeNum) %>%
      filter(Date %in% rebDates)
  }
  
  if (rankType == "1M" & strategyType == "con") {
    d <- 
      d %>% 
      arrange(Date, ret1M) %>%
      group_by(Date) %>%
      mutate(rank1M = row_number()) %>%
      ungroup() %>%
      filter(rank1M <= includeNum) %>%
      filter(Date %in% rebDates)
  }

  if (rankType == "MC") {
    d <- 
      d %>% 
      arrange(Date, desc(MarketCap)) %>%
      group_by(Date) %>%
      mutate(rankMC = row_number()) %>%
      ungroup() %>%
      filter(rankMC <= includeNum) %>%
      filter(Date %in% rebDates)
  }
  
  d <- d %>% select(Date, Name, MarketCap)
  
  result <- split.data.frame(d, d$Date) %>% map(getMcVec)
  
  if (rankType %in% c("1D", "1W", "1M")) {
    result <- 
      result %>% 
      map(function(x) {
        x[names(x)] <- 1/length(x)
        return(x)
      })
  }
  
  return(result)
  
}
