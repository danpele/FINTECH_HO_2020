if (0) {
  
  # target final set of parameters
  values4Params <- 
    list(
      values4RB     = c("1 day"   = "1D",
                        "1 week"  = "1W",
                        "1 month" = "1M"),
      values4LB     = c("1 day"   = "1D",
                        "1 week"  = "1W",
                        "1 month" = "1M"),
      values4topN   = c(0.01, 
                        #2, 3, 4, 
                        0.05, 0.10, 0.25, 0.50),
      values4TC     = c(0.005, 0.01, 0.02),
      values4DFL    = c(0.5, 1, 2),
      strategyTypes = c("mom", "con")
    )
  
} else {
  
  # experimental set of parameters
  values4Params <- 
    list(
      values4RB     = c("1 day"   = "1D",
                        "1 week"  = "1W",
                        "1 month" = "1M"),
      values4LB     = c("1 day"   = "1D",
                        "1 week"  = "1W",
                        "1 month" = "1M"),
      values4topN   = c(# 0.01, 
                        # 0.02, 0.03, 0.04, 
                        # 0.05, 
                        # 0.10, 
                        0.25, 
                        0.50
                        ),
      values4TC     = c(#0.005, 
                        0.01, 
                        0.02
                        ),
      values4DFL    = c(0.5, 
                        1# , 
                        #2
                        ),
      strategyTypes = c("mom", "con")
    )
  
}


