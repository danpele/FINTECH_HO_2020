start_date <- as.Date("2013-12-30")

# load full long data with crypto prices
crypto <- 
  read_rds("data/crypto/data_crypto.rds") %>%
  mutate(Name = ifelse(substr(Name, 1, 1) == " ",
                       substr(Name, 2, nchar(Name)),
                       Name)) %>%
  filter(!(Name %in% c("Aphroditecoin", 
                       "SpainCoin", 
                       "The DAO", 
                       "Electric",
                       "HempCoin",
                       "DubaiCoin",
                       "Scotcoin",
                       "Spots",
                       "EncryptoTel [..."))) %>%
  filter(!(Name == "Bytecoin" & Symbol == "BTE")) %>%
  arrange(Name, Date) %>%
  group_by(Name) %>%
  mutate(obs = row_number()) %>%
  ungroup() %>%
  filter(Date >= start_date) %>%
  arrange(Date, MarketCap %>% desc(), Volume %>% desc(), Name) %>%
  select(Date, Name, Symbol, Price, MarketCap, obs) %>%
  filter(Date <= stop_date) 

# allowed are only crypto with at least 61 historical price observations 
crypto <-
  crypto %>%
  filter(obs > 60)

# removing coins with short history
namesToRemove <- 
  crypto %>% 
  group_by(Name) %>% 
  summarize(n = n()) %>%
  arrange(n) %>%
  filter(n <= 60) %>%
  select(Name) %>%
  pull()

crypto <- crypto %>% filter(!(Name %in% namesToRemove))
rm(namesToRemove)

# filter out topN crypto with largest market cap for every day
topN <- 100
crypto_topN <-
  crypto %>%
  arrange(Date, MarketCap %>% desc()) %>%
  group_by(Date) %>%
  filter(row_number() <= topN) %>%
  ungroup() 

crypto 
crypto_topN

# crypto names
crypto_topN_names <-
  crypto_topN %>% 
  select(Name) %>% arrange() %>% pull() %>% unique()
crypto_topN_names

# filtering out cryptos out of topN range
crypto <-
  crypto %>%
  filter(Name %in% crypto_topN_names)

# wide crypto prices
wcrypto <-
  crypto %>%
  select(-Symbol, -MarketCap, -obs) %>%
  spread(key = Name, value = Price)
wcrypto

# VERIFICATION
wcrypto %>%
  select(Date) %>% pull %>% xts::diff.xts(.) %>% table(useNA = "always")
wcrypto %>% nrow()
# VERIFICATION: OK!

# reading regular assets prices
regular <- 
  read_rds("data/regular/spx.rds") %>%
  mutate(Name = "S&P500", Symbol = "SPX", Price = Close) %>%
  select(Date, Name, Symbol, Price) %>%
  bind_rows(read_rds("data/regular/ndq.rds") %>%
              mutate(Name = "NASDAQ", Symbol = "NDQ", Price = Close) %>%
              select(Date, Name, Symbol, Price)) %>%
  bind_rows(read_rds("data/regular/dax.rds") %>% 
              mutate(Name = "DAX", Symbol = "DAX", Price = Close) %>%
              select(Date, Name, Symbol, Price)) %>%
  bind_rows(read_rds("data/regular/nkx.rds") %>%
              mutate(Name = "NIKKEI", Symbol = "NKX", Price = Close) %>%
              select(Date, Name, Symbol, Price)) %>%
  bind_rows(read_rds("data/regular/kospi.rds") %>%
              mutate(Name = "KOSPI", Symbol = "KSP", Price = Close) %>%
              select(Date, Name, Symbol, Price)) %>%
  bind_rows(read_rds("data/regular/cac.rds") %>%
              mutate(Name = "CAC40", Symbol = "CAC", Price = Close) %>%
              select(Date, Name, Symbol, Price)) %>%
  bind_rows(read_rds("data/regular/x.f.rds") %>%
              mutate(Name = "FTSE100", Symbol = "FTS", Price = Close) %>%
              select(Date, Name, Symbol, Price)) %>%
  bind_rows(read_rds("data/regular/usdeur.rds") %>%
              mutate(Name = "USDEUR", Symbol = "EUR", Price = Close) %>%
              select(Date, Name, Symbol, Price)) %>%
  bind_rows(read_rds("data/regular/usdgbp.rds") %>%
              mutate(Name = "USDGBP", Symbol = "GBP", Price = Close) %>%
              select(Date, Name, Symbol, Price)) %>%
  bind_rows(read_rds("data/regular/usdjpy.rds") %>%
              mutate(Name = "USDJPY", Symbol = "JPY", Price = Close) %>%
              select(Date, Name, Symbol, Price)) %>%
  bind_rows(read_rds("data/regular/usdkrw.rds") %>%
              mutate(Name = "USDKRW", Symbol = "KRW", Price = Close) %>%
              select(Date, Name, Symbol, Price)) %>%
  arrange(Date) %>%
  filter(Date >= start_date,
         Date <= stop_date)

# wide regular prices
wregular <-
  regular %>% select(-Symbol) %>%
  spread(key = Name, value = Price) %>%
  mutate(CAC40   = CAC40 / USDEUR,
         DAX     = DAX / USDEUR,
         FTSE100 = FTSE100 / USDGBP,
         KOSPI   = KOSPI / USDKRW,
         NIKKEI  = NIKKEI / USDJPY) %>%
  select(-USDEUR, -USDGBP, -USDJPY, -USDKRW)

# join WIDE crypto and regular data  
wdata <-
  full_join(wcrypto, wregular) %>%
  arrange(Date) %>%
  zoo::na.locf(na.rm = F) # don't remove leading NAs
wdata

# VERIFICATION
wdata %>%
  select(Date) %>% pull %>% xts::diff.xts(.) %>% table(useNA = "always")
wdata %>% nrow()
# VERIFICATION: OK!

# LONG data with prices for crypto and regular assets
ldata <-
  wdata %>%
  gather(key = Name, value = Price, -Date) %>%
  arrange(Date, Name)

# wide data with returns for both regular and crypto
source("funs/fun-getSimpleReturn.R")
wrets <-
  wdata %>%
  mutate_at(., vars(-Date), getSimpleReturn)

# crypto - adding 1D/1W/1M returns
crypto <-
  crypto %>%
  arrange(Name, Date) %>%
  group_by(Name) %>%
  mutate(
    ret1D = diff.xts(Price, lag = 1) /  lag.xts(Price, k = 1),
    ret1W = diff.xts(Price, lag = 7) /  lag.xts(Price, k = 7),
    ret1M = diff.xts(Price, lag = 30) / lag.xts(Price, k = 30)  
  ) %>%
  ungroup()

# crypto - adding 1D returns rank
crypto <-
  crypto %>%
  arrange(Date, desc(ret1D)) %>%
  group_by(Date) %>%
  mutate(rank1D = row_number()) %>%
  ungroup()

# crypto - adding 1W returns rank
crypto <-
  crypto %>%
  arrange(Date, desc(ret1W)) %>%
  group_by(Date) %>%
  mutate(rank1W = row_number()) %>%
  ungroup()

# crypto - adding 1M returns rank
crypto <-
  crypto %>%
  arrange(Date, desc(ret1M)) %>%
  group_by(Date) %>%
  mutate(rank1M = row_number()) %>%
  ungroup()

# crypto - adding MarketCap rank
crypto <-
  crypto %>%
  arrange(Date, desc(MarketCap)) %>%
  group_by(Date) %>%
  mutate(rankMC = row_number()) %>%
  ungroup()


# saving all tibbles with prices and returns into final rds files
crypto            %>% saveRDS("data/crypto/crypto.rds")
crypto_topN       %>% saveRDS("data/crypto/crypto_topN.rds")
crypto_topN_names %>% saveRDS("data/crypto/crypto_topN_names.rds")
wcrypto           %>% saveRDS("data/crypto/wcrypto.rds")
regular           %>% saveRDS("data/regular/regular.rds")
wregular          %>% saveRDS("data/regular/wregular.rds")
wdata             %>% saveRDS("data/wdata.rds")
ldata             %>% saveRDS("data/ldata.rds")
wrets             %>% saveRDS("data/wrets.rds")
