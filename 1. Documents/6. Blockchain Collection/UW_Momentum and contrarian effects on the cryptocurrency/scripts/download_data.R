### defining functions
source("funs/fun-getCryptoPricesForGivenDate.R")
source("funs/fun-updateCryptoPrices.R")
source("funs/fun-getDataFromStooq.R")


### updating data on crypto
updateCryptoPrices(start.date = last_date,
                   stop.date  = stop_date,
                   updateLongTibble = T)

### updating data on equity indices
getDataFromStooq("^spx")   # S&P500
getDataFromStooq("^ndq")   # NASDAQ
getDataFromStooq("^dax")   # DAX
getDataFromStooq("^nkx")   # NIKKEI
getDataFromStooq("x.f")    # FTSE 100  
getDataFromStooq("^kospi") # KOSPI
getDataFromStooq("^cac")   # CAC40

getDataFromStooq("usdeur")   # USDEUR
getDataFromStooq("usdjpy")   # USDJPY
getDataFromStooq("usdgbp")   # USDGBP
getDataFromStooq("usdkrw")   # USDKRW

getDataFromStooq("ukousd3m")   # LIBOR USD 3M

