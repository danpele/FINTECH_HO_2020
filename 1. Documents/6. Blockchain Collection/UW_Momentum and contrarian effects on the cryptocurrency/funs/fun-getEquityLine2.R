getEquityLine <- function(quotes, 
                          weightsList, 
                          proportionalCost, 
                          initialEquity, 
                          DFL){
  
  # Calculate initial observations of equity lines.
  equityLine <- 
    quotes %>%
    filter(Date < names(weightsList)[1] %>% as.Date()) %>%
    select(Date) %>%
    arrange(Date) %>%
    distinct() %>%
    mutate(equityLine = initialEquity) %>%
    bind_rows(
      data.frame(
        Date = names(weightsList)[1] %>% as.Date(),
        equityLine = initialEquity * ifelse(weightsList[[1]] %>% sum() > 0,
                                            (1 - DFL * proportionalCost),
                                            1)
      )
    )
  
  # Calculate equity line in consecutive steps.

  for (i in 1:(length(weightsList))) {
    # print(i)
    startDate <- names(weightsList)[i] %>% as.Date() + 1
    
    if (i < length(weightsList)) {
      endDate   <- names(weightsList)[i + 1] %>% as.Date()
    } else {
      endDate   <- quotes %>% select(Date) %>% pull() %>% max()
    }
    
    currentQuotes <- quotes %>%
      filter(Date >= startDate - 1 &
               Date <= endDate &
               Name %in% names(weightsList[[i]])) %>%
      spread(Name, Price)
      
    # Ensure proper order.
    currentQuotes <- currentQuotes[c("Date", names(weightsList[[i]]))]
    
    currentEquity <-
      equityLine[equityLine$Date == startDate - 1, ]$equityLine

    # Include rebalancing costs.
    if (i > 1) {
      # Calculate change of positions.
      positionChange <-
        suppressMessages(full_join(((weightsList[[i]] * DFL * currentEquity) /
                                      currentQuotes[1, -1]),
                                   positions))

      positionChange[is.na(positionChange)] <- 0
      if(nrow(positionChange) == 2) {
        positionChange <- abs(positionChange[1, ] - positionChange[2, ])
        
        startQuotes <- quotes %>%
          filter(Date == startDate &
                   Name %in% names(positionChange)) %>%
          spread(Name, Price)
        
        # Ensure proper order and skip dead cryptocurrencies.
        positionChange <- positionChange[names(startQuotes)[-1]]
        startQuotes    <- startQuotes[names(positionChange)]
        
        # Calculate posts based on current prices and positions change.
        costs          <- sum(abs(positionChange) * startQuotes) * proportionalCost
      } else {
        costs <- 0
      }
      
    } else {
      costs <- 0
    }
    
    currentEquity <- currentEquity - costs
    
    # Calculate positions.
    positions     <- (weightsList[[i]] * DFL * currentEquity) / currentQuotes[1, -1]
    freeBalance   <- ifelse(sum(positions) == 0, currentEquity, currentEquity * (1 - DFL))
    
    # Calculate equity line.
    currentEquityLine <- currentQuotes %>%
      select(Date) %>%
      mutate(equityLine = (currentQuotes[, -1] %>%
                             as.matrix()) %*%
               (positions %>%
                  as.numeric()),
             equityLine = equityLine + freeBalance) 
    
    equityLine <- equityLine %>%
      bind_rows(currentEquityLine[-1, ]) %>%
      arrange(Date) %>%
      mutate(equityLine = equityLine %>% zoo::na.locf())
  }
  
  if(any(equityLine$equityLine <= 0)){
    bankruptcyDate <- equityLine %>%
      filter(equityLine <= 0) %>%
      select(Date) %>%
      pull() %>%
      min()
    
    equityLine %>%
      mutate(equityLine = ifelse(Date >= bankruptcyDate, 0, equityLine))
  } else {
    equityLine  
  }
  
}
