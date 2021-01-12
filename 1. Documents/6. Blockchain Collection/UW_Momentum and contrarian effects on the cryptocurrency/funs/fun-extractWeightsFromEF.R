extractWeightsFromEF <- function(x, 
                                 pType = "maxSR",
                                 rfr = 0) {
  
  stopifnot(pType %in% c("maxSR", "minVariance", "maxReturn"))
  
  maxSRP <- 
    x$frontier[, 1:ncol(x$frontier)] %>% 
    as_tibble() %>% 
    mutate(sr = (mean - rfr) / StdDev) %>%
    filter(sr == max(sr)) %>%
    head(., 1) %>%
    select(-out, -sr) %>%
    unlist()
  
  minVarianceP <-
    x$frontier[, 1:ncol(x$frontier)] %>% 
    as_tibble() %>% 
    filter(StdDev == min(StdDev)) %>%
    head(., 1) %>%
    select(-out) %>%
    unlist()
  
  maxReturnP <-
    x$frontier[, 1:ncol(x$frontier)] %>% 
    as_tibble() %>% 
    filter(mean == max(mean)) %>%
    head(., 1) %>%
    select(-out) %>%
    unlist()
 
  if (pType == "maxSR") {
    result <- maxSRP
  } else if (pType == "minVariance") {
    result <- minVarianceP
  } else if (pType == "maxReturn") {
    result <- maxReturnP
  } else {
    cat("WRONG PORTOFLIO TYPE!\n")
    return(-99);
  }
  
  sr <- (result["mean"] - rfr ) / result["StdDev"]
  names(sr) <- "sr"
  
  result <- result[!(names(result) %in% c("mean", "StdDev"))]
  
  if ( sr <= 0) {
    result[1:length(result)] <- 0
  }
  
  names(result) <- names(result) %>% substr(., 3, 100)
  return(result)
  
}
