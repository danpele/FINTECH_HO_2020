getMcVec <- function(x) {
  names  <- x %>% select(Name) %>% pull()
  values <- x %>% select(MarketCap) %>% pull()
  values <- values/sum(values)
  names(values) <- names
  return(values)
}
