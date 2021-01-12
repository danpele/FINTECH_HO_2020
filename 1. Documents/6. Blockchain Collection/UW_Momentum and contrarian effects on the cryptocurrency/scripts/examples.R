
# REBALANCING DATES ============================================================
RB <- 30
rebalancing_dates <- seq(from = as.Date("2014-05-13"), 
                         to = stop_date, 
                         by = RB) - 1
rebalancing_dates %>% length()


# EXAMPLES WITH WEIGHTS AND EQUITY LINES =======================================

# example with weights
{
  weights_RE_1W_RA_1W_mom <-
    createWeights(rebDates     = rebalancing_dates,
                  cdata        = crypto,
                  rankType     = "1W",
                  strategyType = "mom",
                  includePct   = 0.25)
  weights_RE_1W_RA_1W_con <-
    createWeights(rebDates     = rebalancing_dates,
                  cdata        = crypto,
                  rankType     = "1W",
                  strategyType = "con",
                  includePct   = 0.25)
  weights_RE_1W_McW <-
    createWeights(rebDates     = rebalancing_dates,
                  cdata        = crypto,
                  rankType     = "MC",
                  includePct   = 1 )
  weights_RE_1W_EqW <-
    createWeights(rebDates     = rebalancing_dates,
                  cdata        = crypto,
                  rankType     = "1W",
                  strategyType = "mom",
                  includePct   = 1)
}

# example with equity lines
{
  eqL_RE_1W_RA_1W_mom <-
    getEquityLine(quotes = ldata, 
                  weightsList = weights_RE_1W_RA_1W_mom, 
                  proportionalCost = 0.01, 
                  initialEquity = 1000, 
                  DFL = 1) %>%
    filter(Date >= "2014-05-01")
  eqL_RE_1W_RA_1W_con <-
    getEquityLine(quotes = ldata, 
                  weightsList = weights_RE_1W_RA_1W_con, 
                  proportionalCost = 0.01, 
                  initialEquity = 1000, 
                  DFL = 1) %>%
    filter(Date >= "2014-05-01")
  eqL_RE_1W_McW <-
    getEquityLine(quotes = ldata, 
                  weightsList = weights_RE_1W_McW, 
                  proportionalCost = 0.01, 
                  initialEquity = 1000, 
                  DFL = 1) %>%
    filter(Date >= "2014-05-01")
  eqL_RE_1W_EqW <-
    getEquityLine(quotes = ldata, 
                  weightsList = weights_RE_1W_EqW, 
                  proportionalCost = 0.01, 
                  initialEquity = 1000, 
                  DFL = 1) %>%
    filter(Date >= "2014-05-01")
}

# EQUITY LINES - visualizing ===================================================

plot.data <-
  wdata %>% select(Date, "S&P500", "Bitcoin") %>%
  left_join(eqL_RE_1W_RA_1W_mom) %>% rename(eqL_RE_1W_RA_1W_mom = equityLine) %>%
  left_join(eqL_RE_1W_RA_1W_con) %>% rename(eqL_RE_1W_RA_1W_con = equityLine) %>%
  left_join(eqL_RE_1W_McW) %>% rename(eqL_RE_1W_McW = equityLine) %>%
  left_join(eqL_RE_1W_EqW) %>% rename(eqL_RE_1W_EqW = equityLine) %>%
  filter(Date >= "2014-05-01") %>%
  mutate(eqL_RE_1W_RA_1W_mom = eqL_RE_1W_RA_1W_mom / eqL_RE_1W_RA_1W_mom[1],
         eqL_RE_1W_RA_1W_con = eqL_RE_1W_RA_1W_con / eqL_RE_1W_RA_1W_con[1],
         eqL_RE_1W_McW = eqL_RE_1W_McW / eqL_RE_1W_McW[1],
         eqL_RE_1W_EqW = eqL_RE_1W_EqW / eqL_RE_1W_EqW[1],
         `S&P500` = `S&P500` / `S&P500`[1],
         Bitcoin = Bitcoin / Bitcoin[1] 
  ) 

plot.data %>%
  gather(key = asset, value = Price, -Date) %>%
  ggplot(aes(x = Date, y = Price, col = asset)) + 
  geom_line() + 
  theme_bw()

plot.data %>% 
  select(-Date) %>%
  xts::xts(., order.by = plot.data$Date) %>%
  dygraphs::dygraph(.) %>%
  dygraphs::dyRangeSelector(., height = 40) %>%
  dygraphs::dyAxis(., name = "y", logscale = "Log10")

