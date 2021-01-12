## 2020-05-29
## Paweł Sakowski
## Quantitative Finance Research Group (QFRG)
## Faculty of Economic Science, University of Warsaw
##
## This project contains a shiny application which extends results presented
## in Kosc et. al. (2019) "Momentum and contrarian effects on the cryptocurrency 
## market", Physica A 523 (2019) 691-701

rm(list = ls())
gc()

# LOADING PACKAGES =============================================================
library(tidyverse)
library(gapminder)
library(PortfolioAnalytics)
library(ggrepel)
library(transformr)
library(sf)
library(PerformanceAnalytics)

# CHECK DATA +++++==============================================================
source("scripts/check_data.R")

# DOWNLOAD NEW RAW DATA ========================================================
# with crypto and regular assets prices
# directly from stooq.com and coinmarketcap.com
stop_date  <- Sys.Date() - 1
if (0) source("scripts/download_data.R")

# READING, TRANSFORMING, SAVING ================================================
# reading crypto and regular data from updated rds files
# transforming data, saving to final rds files
if (0) source("scripts/transform_data.R")

# LOADING THE DATA =============================================================
# loading all the tibbles with prices and returns from final rds files
if (1) source("scripts/load_data.R")

# CREATING EQUITY LINES ========================================================
# change 0 below into 1 to run the chunk with recalculation of all equity lines
# and overwrite the resulting rds file
if (0) {
  source("funs/fun-getEquityLine.R")
  source("funs/fun-getMcVec.R")
  source("funs/fun-createWeights.R")
  
  source("scripts/setValues4Params.R")
  source("scripts/createStrategyParamsTable.R")
  
  eqLList <- list()
  n <- nrow(strategyParamsTable)
  
  for(i in 1:n) {
    
    cat(i, "/", n, "\n", sep = "")  
    rbFreq <-
      case_when(strategyParamsTable$RB[i] == "1D" ~ 1,
                strategyParamsTable$RB[i] == "1W" ~ 7,
                strategyParamsTable$RB[i] == "1M" ~ 30,
      )
    
    rebalancing_dates <- seq(from = as.Date("2014-06-01"), 
                             to = stop_date - 1, 
                             by = rbFreq) - 1
    
    if (strategyParamsTable$type[i] == "mcw") {
      thisWeights <-
        createWeights(rebDates     = rebalancing_dates,
                      cdata        = crypto,
                      rankType     = "MC",
                      includePct   = 1)
    } else if (strategyParamsTable$type[i] == "eqw") {
      thisWeights <-
        createWeights(rebDates     = rebalancing_dates,
                      cdata        = crypto,
                      rankType     = "1W",
                      strategyType = "mom",
                      includePct   = 1)
    } else {
      thisWeights <-
        createWeights(rebDates     = rebalancing_dates,
                      cdata        = crypto,
                      rankType     = strategyParamsTable$LB[i] %>% as.character(),
                      strategyType = strategyParamsTable$type[i] %>% as.character(),
                      includePct   = strategyParamsTable$topN[i])
    }
  
    eqLList[[i]] <-
      getEquityLine(quotes = ldata, 
                    weightsList = thisWeights, 
                    proportionalCost = strategyParamsTable$TC[i], 
                    initialEquity = 1000, 
                    DFL = strategyParamsTable$DFL[i]) %>%
      filter(Date >= "2014-05-01")
    
    names(eqLList)[i] <-
      paste0("eql_",
             strategyParamsTable$type[i],
             "_RE_",
             strategyParamsTable$RB[i],
             "_LB_",
             strategyParamsTable$LB[i],
             "_topN_",
             strategyParamsTable$topN[i], 
             "_TC_",
             strategyParamsTable$TC[i],
             "_DFL_",
             strategyParamsTable$DFL[i]
      )
    
  }
  
  eqLList %>% saveRDS("data/eqLList.rds")
  
}

# RUN THE APP ==================================================================
shiny::runApp("app")

