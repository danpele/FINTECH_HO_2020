drawRandomPortfolio <- function(r, vcov) {
  rnd_weights <- rgamma(length(r), 0.6)
  rnd_weights <- rnd_weights/sum(rnd_weights)
  names(rnd_weights) <- names(r)
  y_rnd <- drop(rnd_weights %*% r)
  names(y_rnd) <- "mean"
  x_rnd <- drop(rnd_weights %*% vcov %*% rnd_weights ) ^ 0.5
  names(x_rnd) <- "StdDev"
  
  tb <- c(rnd_weights, y_rnd, x_rnd) %>% as.list() %>% as_tibble()
  return(tb)
}
