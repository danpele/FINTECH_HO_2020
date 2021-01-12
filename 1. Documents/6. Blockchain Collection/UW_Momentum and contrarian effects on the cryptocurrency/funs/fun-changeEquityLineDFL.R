changeEquityLineDFL <- function(equityLine, DFL, rebalancingDates, DFLfrom = 1){
  DFL <- DFL / DFLfrom
  
  initialValue  <- equityLine$equityLine[1]
  
  newEquityLine <- equityLine %>%
    filter(Date <= rebalancingDates[2]) %>%
    mutate(equityLine = equityLine * DFL + initialValue * (1 - DFL))
  
  for(i in 2:(length(rebalancingDates) - 1)){
    currentInitialValue <- newEquityLine$equityLine %>% last()
    
      currentEquity <- equityLine %>%
      filter(Date >= rebalancingDates[i] &
               Date <= rebalancingDates[i + 1])
    
      currentEquity <- currentEquity %>%
        mutate(equityLine = currentInitialValue *
                 (DFL * (equityLine / currentEquity$equityLine[1]) + (1 - DFL)))
      
      newEquityLine <- newEquityLine %>% bind_rows(currentEquity[-1, ])
  }
  
  newEquityLine
}
