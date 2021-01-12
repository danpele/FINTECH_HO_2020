
# aRC
getARC <- function(x, scale = 365){
  nInt <- length(x)
  ARC  <- (x[length(x)] / x[1]) ^ (scale/nInt) - 1
  return(ARC)
}

# aSD
getASD <- function(x, scale = 365){
  r <- xts::diff.xts(x)/xts::lag.xts(x)
  annSdtDev <- sd(r, na.rm = T) * sqrt(scale)
  return(annSdtDev)
}


# MD
getMD <- function(x){
  r <- xts::diff.xts(x)/xts::lag.xts(x)
  return(suppressWarnings(PerformanceAnalytics::maxDrawdown(r)))
}


# IR
getIR <- function(x, scale = 365){
  getARC(x, scale)/getASD(x, scale)
}

# LD
getLD2 <- function(x, scale = 365) {
  currentMax <- x[1]
  LD         <- rep(NA, length(x))
  LD[1]      <- 0
  
  for (i in 2:length(x)) {
    if (x[i] >= currentMax) {
      currentMax <- x[i]
      LD[i]      <- 0
    } else {
      LD[i]      <- LD[i - 1] + 1
    }
  }
  
  LD <- LD / scale
  
  # return(round(LD, 2))
  return(LD)
}

# get all performance stats
getPerformanceStats <- function(x, scale = 365){
  
  aRC     <- getARC(x, scale)
  aSD     <- getASD(x, scale)
  MD      <- getMD(x)
  MLD     <- max(getLD2(x, scale))
  IR      <- getIR(x, scale)
  IRMD    <- IR/(MD)
  IRaRCMD <- IR * aRC / MD
  IR2     <- aRC ^ 3 * sign(aRC) / (aSD * MD * MLD)
  nObs    <- length(x)
  
  result <- c(100 * aRC, 100 * aSD, 100 * MD, MLD,
              IR, IRMD, IRaRCMD, IR2, nObs)
  names(result) <- c('aRC', "aSD", "MD", "MLD",
                     "IR", "IRMD", "IRaRCMD", "IR\"", 
                     "nObs")
  return(result)
  
}
