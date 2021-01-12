## function for portfolio optimization 
optimizePortfolio <- function(this.date,
                              topN_data,
                              wide_data,
                              lookback_days = 30,
                              n.port = 25, 
                              includeCrypto = T,
                              regular_names = c("CAC40", "DAX", 
                                                "FTSE100", "KOSPI", 
                                                "NASDAQ", "NIKKEI", 
                                                "S&P500")
){
  
  this.date <- as.Date(this.date)
  
  crypto_names <- 
    topN_data %>%
    filter(Date == this.date) %>%
    select(Name) %>%
    pull()
 
  if (includeCrypto) {
    finalAssetsNames <- c(regular_names, crypto_names)  
  } else {
    finalAssetsNames <- c(regular_names)
  }
 
  # wide data with appropriate columns and observations
  final_assets_wide <-
    wide_data %>%
    select(Date, finalAssetsNames) %>%
    filter(Date <= this.date & Date >= (this.date - lookback_days) )   
  
  # transform wide data into XTS object
  final_assets_wide.xts <- xts::xts(x = final_assets_wide[, -1], 
                                    order.by = final_assets_wide$Date)
  

  ## SETTING UP PORTFOLIO SPECIFICATIONS
  library(PortfolioAnalytics)
  
  this_portfolio <- portfolio.spec(assets = finalAssetsNames)  

  
  ## ADDING CONSTRAINTS TO PORTFOLIO OPTIMIZATIOM 
  ## ** SHOULD BE CHANGED during sensitivity analysis **
  this_portfolio <- add.constraint(portfolio = this_portfolio, 
                                   type      = "weight_sum", 
                                   min_sum   = 0.99, 
                                   max_sum   = 1.01)
  
  this_portfolio <- add.constraint(portfolio = this_portfolio, 
                                   type      = "long_only")
  
  #this_portfolio <- add.constraint(portfolio = this_portfolio,
  #                                 type      = "box",
  #                                 min       = 0.01,
  #                                 max       = 0.6)
  
  this_portfolio <- add.objective(portfolio = this_portfolio, 
                                  type      = "risk", 
                                  name      = "StdDev")
  
  this_portfolio <- add.objective(portfolio = this_portfolio, 
                                  type      = "return", 
                                  name      = "mean")
  
  
  
  opt_portfolio <- optimize.portfolio(R               = final_assets_wide.xts, 
                                      portfolio       = this_portfolio,
                                      optimize_method = "ROI",
                                      trace           = TRUE,
                                      # message         = TRUE,
                                      maxSR           = TRUE) 
  
  ef <- create.EfficientFrontier(R = final_assets_wide.xts,
                                 portfolio = this_portfolio,
                                 match.col = "StdDev",
                                 type = "mean-sd",
                                 n.portfolios = n.port
                                 )
  
  result <- list(opt_portfolio, ef)
  return(result)
}
