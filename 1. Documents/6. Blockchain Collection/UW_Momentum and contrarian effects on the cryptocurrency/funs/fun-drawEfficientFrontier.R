drawEfficientFrontier <- function(pf1 = NULL,
                                  pf0 = NULL,
                                  scale = 365, 
                                  numPortfolios = 1000,
                                  extractWeightsFromEF = T,
                                  yMin = NA, yMax = NA,
                                  xMin = NA, xMax = NA,
                                  savePlot = F,
                                  ...) {
  
  # verification if at least one EF is present
  stopifnot(!(is.null(pf1) & is.null(pf0)))
  
  if (!is.null(pf1)) {
    r1    <- (pf1[[1]]$R %>% colMeans() ) * scale
    vcov1 <- (pf1[[1]]$R %>% cov() ) * scale  
    stdDev1 <- diag(vcov1) ^ 0.5
    if (extractWeightsFromEF) {
      w1 <- pf1[[2]] %>% extractWeightsFromEF(., ...)
    } else {
      w1 <- pf1[[1]] %>% extractWeights()  
    }
    y_opt1 <- as.numeric(w1 %*% r1)
    x_opt1 <- as.numeric(w1 %*% vcov1 %*% w1) ^ 0.5
    
    randomPortfolios1 <-
      lapply(1:numPortfolios, function(x) drawRandomPortfolio(r1, vcov1)) %>% 
      reduce(bind_rows) %>%
      select(StdDev, mean)
    
    points1 <-
      tibble(
        mean   = c(0, y_opt1),
        StdDev = c(0, x_opt1),
        name   = c("RF", "TP"))
    
    ann_frontier1 <- pf1[[2]]$frontier[, c("StdDev", "mean")] %>%
      as_tibble() %>%
      mutate(mean = mean * scale,
             StdDev = StdDev * sqrt(scale)) %>%
      select(mean, StdDev)
    
  }
  
  if (!is.null(pf0)) {
    r0    <- (pf0[[1]]$R %>% colMeans() ) * scale
    vcov0 <- (pf0[[1]]$R %>% cov() ) * scale  
    stdDev0 <- diag(vcov0) ^ 0.5
    if (extractWeightsFromEF) {
      w0 <- pf0[[2]] %>% extractWeightsFromEF(., ...)
    } else {
      w0 <- pf0[[1]] %>% extractWeights()  
    }
    y_opt0 <- as.numeric(w0 %*% r0)
    x_opt0 <- as.numeric(w0 %*% vcov0 %*% w0) ^ 0.5
    
    randomPortfolios0 <-
      lapply(1:numPortfolios, function(x) drawRandomPortfolio(r0, vcov0)) %>% 
      reduce(bind_rows) %>%
      select(StdDev, mean)
    
    points0 <-
      tibble(
        mean   = c(0, y_opt0),
        StdDev = c(0, x_opt0),
        name   = c("RF", "TP"))
    
    ann_frontier0 <- pf0[[2]]$frontier[, c("StdDev", "mean")] %>%
      as_tibble() %>%
      mutate(mean = mean * scale,
             StdDev = StdDev * sqrt(scale)) %>%
      select(mean, StdDev)
  }
 
  if (!is.null(pf1)) {
    assets1 <-
      tibble(
        mean = r1,
        StdDev = stdDev1,
        name = names(r1)
      )
  } 
  
  if (!is.null(pf0)) {
    assets0 <-
      tibble(
        mean = r0,
        StdDev = stdDev0,
        name = names(r0)
      )
  }
   
  
  # === PLOT ===
  p <- ggplot() + theme_bw()
  
  if (!is.null(pf1)) p <- p +
    geom_point(aes(x = StdDev, y = mean),
               size = 1.2,
               color = "black",
               fill = "gray",
               alpha = 0.5,
               shape = 21,
               data = randomPortfolios1)
  
  if (!is.null(pf0)) p <- p +
    geom_point(aes(x = StdDev, y = mean),
               size = 1.2,
               color = "black",
               fill = "yellow3",
               alpha = 0.5,
               shape = 21,
               data = randomPortfolios0)
  
  if (!is.null(pf1)) p <- p +
    geom_point(aes(x = StdDev, y = mean),
               size = 0.5, 
               data = ann_frontier1) + 
    geom_line(aes(x = StdDev, y = mean),
              data = ann_frontier1) + 
    geom_abline(intercept = 0, 
                slope = y_opt1/x_opt1, color = "blue", 
                linetype = "solid", size = 0.5)
  
  if (!is.null(pf0)) p <- p +
    geom_point(aes(x = StdDev, y = mean),
               size = 0.5, 
               data = ann_frontier0) + 
    geom_line(aes(x = StdDev, y = mean),
              data = ann_frontier0) + 
    geom_abline(intercept = 0, 
                slope = y_opt0/x_opt0, color = "lightblue", 
                linetype = "solid", size = 0.5)
  
  if (!is.null(pf1)) p <- p +
    geom_point(aes(x = StdDev, y = mean, fill = name),
               size = 3,
               alpha = 0.85,
               shape = 21,
               data = points1) + 
    guides(fill = FALSE) + 
    scale_fill_brewer(palette = "Dark2") + 
    geom_label_repel(aes(x = StdDev, y = mean, label = name),
                     size = 2.5,
                     data = points1)
  
  if (!is.null(pf0)) p <- p +
    geom_point(aes(x = StdDev, y = mean, fill = name),
               size = 3,
               alpha = 0.85,
               shape = 21,
               data = points0) + 
    guides(fill = FALSE) + 
    scale_fill_brewer(palette = "Dark2") + 
    geom_label_repel(aes(x = StdDev, y = mean, label = name),
                     size = 2.5,
                     data = points0)
  
  if (!is.null(pf1)) p <- p +
    geom_point(aes(x = StdDev, y = mean),
               size = 3,
               fill = c("magenta4"),
               alpha = 0.85,
               shape = 21,
               data = assets1) + 
    geom_label_repel(aes(x = StdDev, y = mean, label = name),
                     size = 2.5,
                     data = assets1)
  
  if (!is.null(pf0) & is.null(pf1)) p <- p +
    geom_point(aes(x = StdDev, y = mean),
               size = 3,
               fill = c("magenta4"),
               alpha = 0.85,
               shape = 21,
               data = assets0) + 
    geom_label_repel(aes(x = StdDev, y = mean, label = name),
                     size = 2.5,
                     data = assets0)
  
  if (!is.null(yMin) & !is.null(yMax)) p <- p
  if (!is.null(xMin) & !is.null(xMax)) p <- p
  
  p <- p +
    scale_y_continuous(labels = function(x) paste0(x*100, "%"),
                       limits = c(yMin, yMax)) +
    scale_x_continuous(labels = function(x) paste0(x*100, "%"),
                       limits = c(xMin, xMax)) +
    xlab("standard deviation of daily returns") +
    ylab("mean of daily returns")
  
  
  
  show(p)
  if (savePlot) {
    ggsave(p, filename = "out/img/plot.png")
    ggsave(p, filename = "out/img/plot.pdf")
  }
  
  return(p)
  
  # ggplotly(p)
  #
  # max(pf[[2]]$frontier[,"mean"] / pf[[2]]$frontier[,"StdDev"])
  # pf[[1]]$objective_measures$mean / pf[[1]]$objective_measures$StdDev
  
}
