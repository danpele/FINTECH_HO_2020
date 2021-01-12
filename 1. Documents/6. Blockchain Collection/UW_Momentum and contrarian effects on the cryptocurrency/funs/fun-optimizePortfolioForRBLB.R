## function for portfolio optimization for given rabalancing frequency
optimizePortfolioForRBLB <- function(RB = "1 month",
                                     LB = 30,
                                     saveResults = T,
                                     includeCrypto = T,
                                     ...){
  
  stopifnot(RB %in% c("1 day", "2 days", "3 days", 
                      "4 days", "5 days", "6 days",
                      "1 week", "2 weeks", "3 weeks", 
                      "1 month", "2 months", "3 months", 
                      "4 months", "5 months", "6 months"))
  
  ## defining function for portfolio optimization at given rebalancing dates
  source("src/fun-optimizePortfolio.R")
  
  ## generating rebalacing dates
  rebalancing_dates <- seq(from = as.Date("2014-07-01"), 
                           to   = as.Date("2019-09-20"),
                           by   = RB) - 1
  rebalancing_dates %>% length()
  
  ## creating empty list which will store weights for all rebalacing dates
  opt_portfolio_list <- list()
  
  ## performing portfolio optimization for all rebalancing dates
  for (i in 1:length(rebalancing_dates)) {
    cat("optimization #", i, "/", length(rebalancing_dates), "\n", sep = "")
    opt_portfolio_list[[i]] <- 
      optimizePortfolio(this.date  = rebalancing_dates[i],
                        lookback_days = LB,
                        includeCrypto = includeCrypto,
                        ...)
  }
  
  ## assigning names to list elements 
  names(opt_portfolio_list) <- rebalancing_dates
  
  RBabb <- substr(RB, 1, 3) %>% gsub(" ", "", .)
  if (includeCrypto) {
    CRabb <- "C1"
  } else {
    CRabb <- "C0"
  }
  
  
  fileName <- paste0("out/optPortfolios/opt_portfolio_list_",
                     "RB_", RBabb,
                     "_",
                     "LB_", LB,
                     "d_",
                     CRabb,
                     ".rds")
  
  ## saving list with optimized poftfolios to rds file
  if (saveResults) opt_portfolio_list %>% saveRDS(fileName)

}
