#####################################################################
#                    Data Wrangling
#        Get, Clean and Transform of the Data
#####################################################################
# Source CRPTg3_DSv3.R


library(reshape2)
library(data.table)
library(xtable)

#Setting of the folder with the datasets
setwd('D:/DOCTORADO/Scripts/R/Revision1/')
source('CryptFunctionsV10.R')

CRYP.DS <- read.table("Dataset/DS_CryptoMK2018.csv",header=TRUE, sep=",") #10134612

#Format and datatype conversions:
CRYP.DS$time<-as.Date(CRYP.DS$time) 
CRYP.DS$close<-as.double(CRYP.DS$close)
CRYP.DS$volumefrom<-as.double(CRYP.DS$volumefrom)
CRYP.DS$volumeto<-as.double(CRYP.DS$volumeto)
CRYP.DT <- as.data.table(CRYP.DS) #10134612

##################################### VARIABLE DESCRIPTIONS ######################################
#https://www.cryptocompare.com/coins/guides/glossary-of-trading-terms/
#https://ccc-api.cloudapp.net/faq
# Variables: all prices in USD

#Volume "From" and “To” are the volumes of the respective currency pair.
#Volumefrom is the total number of units of the asset specified by fsym
#  The total amount of the base currency traded into the quote currency during 
#  this period of time (in units of the base currency). 
# it is the volume in the base currency that things are traded into.

#Volumeto is the worth/value of the total number of units of the asset specified by tsym.
# it is the volume in the currency that is being traded

# Lets say you are looking at the Doge/BTC market.
# the volumeto is the Doge volume for example 1,000,000
# the VOLUMEFROM is the BTC volume which if we assume Doge price was 100 fixed it will be 10,000

#Cryptocompare glossary: https://www.cryptocompare.com/coins/guides/glossary-of-trading-terms/
#For example, for the BTC-USD pair  “Volume From” is the number of Bitcoins traded for US dollars 
#while “Volume To” is the number of dollars traded (for the period) for Bitcoins.

#“Volume From” and “To” are the volumes of the respective currency pair.
#For example, for the BTC-USD pair  “Volume From” is the number of Bitcoins traded for US dollars 
#while “Volume To” is the number of dollars traded (for the period) for Bitcoins.
#XRP-USD pair VOLUMETO means the volume in the currency that is being traded (i.e. USD)
#XRP-USD pair VOLUMEFROM means the volume in the base currency that things are traded into (i.e. XRP).

# close: daily close price 
# high: daily highest price
# open: daily open price
# low: daily lowest price

#Marketcap:
#When calculating Market Cap, we account for all coins in circulation, including those held by team 
#members or the company. This also includes coins in smart contracts or escrow. If the coins have been 
#issued and have not been burned, they will be accounted for in the Market Cap. Market Cap is calculated 
#by multiplying Current Supply with the current price.

#SYM: cryptocurrency ticket

# ##################  OUTLIERS TREATMENT and TIMEFRAME FILTERING ################################

# CLEANING 1: duplicated registers
CRYP.DT.Filtered<- unique(CRYP.DT) #10131981
#Gross return: Rg(t)=S(t)/S(t-1)
#Continous compound return: rend = r(t) = log(S(t)/S(t-1)) = log(S(t)) - log(S(t-1));
#Simple Net return: Rt(t)=S(t)/S(t-1) - 1 = exp(Rg(t)) - 1
CRYP.RT.short <- CRYP.DT.Filtered[,.(time, close, volumefrom, volumeto, rend = log(close)-log(close[-1]),
                                  Rt = (close - close[-1])/close[-1]), by=SYM]

#CRYP.DT.Filtered[CRYP.DT.Filtered$SYM=='ETH',]
### Market cap and Volume variables declaration on the table ################################
#Volumefrom is the total number of units of the asset specified by fsym. 
#Volumeto is the worth/value of the total number of units of the asset specified by tsym.
setnames(CRYP.RT.short,c('volumefrom','volumeto'),c('volumeUSD','volume'))

# CLEANING 2: Remove of NaN registers
CRYP.RT.short <- na.omit(CRYP.RT.short) #1554622
CRYP.RT.short[CRYP.RT.short$SYM=='BTC',]

# CLEANING 3: Remove of Rend with Inf values 
CRYP.RT.short <- CRYP.RT.short[is.finite(rend),] #1549409 / 7 variables

# FILTER 1: Only registers of 2018
CRYP.RT.short <- CRYP.RT.short[time >='2017-12-31' & time <='2018-12-31'] #798708 / 7 variables


# FILTER 2: Only cryptoasset that were on the market most of the days along all the year 2018 taken BTC as reference 
Crypt19 <- CRYP.RT.short[,.N,by=SYM] #2565
Crypt19$SYM <- as.character(Crypt19$SYM)
Nmin <- Crypt19[SYM=='BTC']$N
Crypt19 <- Crypt19[N==Nmin] #1723 Cryptocurrencies avaiables on the market most of the days along 2018
CRYP.RT.short <- merge(CRYP.RT.short,Crypt19, by="SYM")
CRYP.RT.short <- CRYP.RT.short[,.(SYM, time, close, volumeUSD, volume, rend, Rt)] #630618
CRYP.RT.short[CRYP.RT.short$SYM=='BTC',]
############### NEW AFTER FIRST REVISSION ###################################################

### Transactions occurs
Transac <- CRYP.RT.short[volume>0,.N,(SYM)]
names(Transac)<-c('SYM','TradDays')
NonTransac<- CRYP.RT.short[volume==0,.N,.(SYM)]
names(NonTransac)<-c('SYM','nonTradDays')
NonTransac <- NonTransac[order(-nonTradDays)]
nrow(NonTransac[nonTradDays==365,])
#hist(Transac$N, freq = TRUE, breaks = 150)
hist(NonTransac$nonTradDays, freq = TRUE, breaks = 150)

ggplot(data=NonTransac, aes(x=NonTransac$nonTradDays)) + 
        geom_histogram(breaks=seq(20, 400, by=2), 
                       col="red", 
                       fill="green") + 
        labs(title="Histogram for non-Trading days", x="non-Trading days", y="Count") + 
        xlim(c(0,366)) + 
        ylim(c(0,350))


### Heavy tail analysis
library(poweRlaw)

allcrypt <- unique(CRYP.RT.short$SYM)
PLD <- data.frame()
i=0
for (crypt in allcrypt){
        i=i+1
        print(crypt)
        print(i)
        rend <- CRYP.RT.short[SYM %in% crypt]$rend
        rend.nor <- Norm(rend)
        if ((sum(rend==0)/length(rend)) > 0.95) next
        rend.pos <- rend.nor[rend.nor>0] #Right tail
        rend.neg <- abs(rend.nor[rend.nor<0]) #Left tail
        
        if ((length(rend.pos) < 3) | (length(rend.neg) < 3)) next
        if ((sum(rend.pos==0)/length(rend.pos)) > 0.95 | (sum(rend.neg==0)/length(rend.neg)) > 0.95) next
        
        # Hill2 estimator
        #Positive tail
        m_bl =conpl$new(rend.pos)
        est = estimate_xmin(m_bl)
        m_bl$setXmin(est)
        hill2.p <- estimate_pars(m_bl)
        sd.p <- (hill2.p$pars - 1)/sqrt(length(rend.pos))
        
        #Negative tail
        m_bl =conpl$new(rend.neg)
        est = estimate_xmin(m_bl)
        m_bl$setXmin(est)
        hill2.n <- estimate_pars(m_bl)
        sd.n <- (hill2.n$pars - 1)/sqrt(length(rend.neg))
        
        #Power Law Distribution (PLD)
        PLD <- rbind(PLD,data.frame(crypt, hill2.p$pars, sd.p, hill2.n$pars, sd.n))
}

temporal2 <- PLD
names(PLD)<-c('SYM','AlphaP','Sd.P','AlphaN','Sd.N')
PLD.HQ <- PLD[PLD$AlphaP>2 & PLD$AlphaN>2,] #983
nrow(PLD.HQ)

TransacPLD <- merge(Crypt19, Transac, all.x = TRUE)
TransacPLD <- merge(TransacPLD,NonTransac, all.x = TRUE)
TransacPLD <- merge(TransacPLD, PLD, all.x = TRUE)
TransacPLD <- TransacPLD[,c(-2)]

nrow(TransacPLD[TransacPLD$nonTradDays==366,]) #306 number of cryptocurrencies no traded during 2018
nrow(TransacPLD[TransacPLD$nonTradDays==365,]) #18

write.table(TransacPLD, file = "Tables/TransaccPLD.txt", sep = "\t",
           row.names = TRUE, col.names = NA)

TranscPLd.landscape <- cbind(TransacPLD[1:150,],TransacPLD[151:300,])
TranscPLd.landscape <- cbind(TranscPLd.landscape,TransacPLD[301:350,])
temp1 <- TranscPLd.landscape
temp1 <- data.frame(temp1)

row.names(temp1)<- NULL

xtable(temp1, digits = 4, include.rownames=FALSE)

TranscPLd.landscape <- cbind(TransacPLD[351:500,],TransacPLD[501:650,])
TranscPLd.landscape <- cbind(TranscPLd.landscape,TransacPLD[651:750,])
temp2 <- TranscPLd.landscape
rownames(temp2) <- seq(151,300)

xtable(temp2, digits = 4, include.rownames=FALSE)

TranscPLd.landscape <- cbind(TransacPLD[751:900,],TransacPLD[901:1050,])
TranscPLd.landscape <- cbind(TranscPLd.landscape,TransacPLD[1051:1150,])
temp3 <- TranscPLd.landscape
rownames(temp3) <- seq(301,450)

xtable(temp3, digits = 4, include.rownames=FALSE)

TranscPLd.landscape <- cbind(TransacPLD[1151:1300,],TransacPLD[1301:1450,])
TranscPLd.landscape <- cbind(TranscPLd.landscape,TransacPLD[1451:1600,])
temp4 <- TranscPLd.landscape
rownames(temp4) <- seq(451,600)

xtable(temp4, digits = 4, include.rownames=FALSE)

TranscPLd.landscape <- cbind(TransacPLD[1601:1640,],TransacPLD[1641:1680,])
TranscPLd.landscape <- cbind(TranscPLd.landscape,TransacPLD[1681:1723,])
temp5 <- TranscPLd.landscape
rownames(temp5) <- seq(601,643)

xtable(temp5, digits = 4, include.rownames=FALSE)

#############################################################################################

###################################################################################################
####             Table transformations: from long-format to wide-format                        ####
####             Variables: CLOSE, VOLUME and REND                                             ####
####Comments: some of the following tables will be used by the different clustering R functions####
###################################################################################################


CRYP.wDT.close <- dcast.data.table(CRYP.RT.short, time ~ SYM, value.var = "close")
CRYP.wDT.volume <- dcast.data.table(CRYP.RT.short, time ~ SYM, value.var ="volume")
CRYP.wDT.rend <- dcast.data.table(CRYP.RT.short, time ~ SYM, value.var ="rend")

CRYP.wDF.close <- as.data.frame(CRYP.wDT.close) 
CRYP.wDF.volume <- as.data.frame(CRYP.wDT.volume) 
CRYP.wDF.rend <- as.data.frame(CRYP.wDT.rend) 

CRYP.timeWise.rend <- t(CRYP.wDT.rend)
colnames(CRYP.timeWise.rend) <- CRYP.timeWise.rend[1,]
CRYP.timeWise.rend <- CRYP.timeWise.rend[-1,]
CRYP.timeWise.price <- t(CRYP.wDT.close)
colnames(CRYP.timeWise.price) <- CRYP.timeWise.price[1,]
CRYP.timeWise.price <- CRYP.timeWise.price[-1,]

temp1 <- unlist(CRYP.timeWise.rend)
temp2 <- as.numeric(temp1)
temp3 <- matrix(temp2, byrow = TRUE, nrow = nrow(temp1))
colnames(temp3) <- colnames(CRYP.timeWise.rend)
rownames(temp3) <- rownames(CRYP.timeWise.rend) 
CRYP.timeWise.rend <- temp3

temp1 <- unlist(CRYP.timeWise.price)
temp2 <- as.numeric(temp1)
temp3 <- matrix(temp2, byrow = TRUE, nrow = nrow(temp1))
colnames(temp3) <- colnames(CRYP.timeWise.price)
rownames(temp3) <- rownames(CRYP.timeWise.price) 
CRYP.timeWise.price <- temp3


#We generate a list of each cryptocurrency
n1 <- nrow(Crypt19)
CRYP.LST <- lapply(1:n1,funCrypto)


##################################   Format adaptation  ########################################
library('xts')
CRYPrice_xts <- as.xts(CRYP.wDT.close) #Returns table in XTS format (requires xts library)


###############################################################################################
#                        Treatment of cryptocurrency description file                         #
###############################################################################################
#File 1: One sample of information of each cryptocurrency
COINTYPE19.DS <- read.table("Dataset/Coins2019FINAL") #97347
#COINTYPE19.DS <- read.table("Dataset/Coins2018.txt") #41004
COINTYPE19.DT <- as.data.table(COINTYPE19.DS)

TypeField <- grep('TYPE',COINTYPE19.DT$V1)
COINTYPE19.DT[TypeField]=NA
COINTYPE19.DT <- na.omit(COINTYPE19.DT) #94716 --> #39865

SymbolField <- grep('FROMSYMBOL',COINTYPE19.DT$V1)

seqSimb <- c()
toString(seqSimb)

for (i in SymbolField){
        simbolo <- rep(toString(COINTYPE19.DT$V2[i]),36)
        seqSimb <- append(seqSimb, simbolo)
}

COINTYPE19.DT$V3 <- seqSimb
CRYPTO19.DT <- dcast.data.table(COINTYPE19.DT, V3 ~ V1, value.var='V2') #2631 obs.
indx <- colnames(CRYPTO19.DT)
CRYPTO19.DT$MKTCAP <- as.numeric(as.character(CRYPTO19.DT$MKTCAP)) 

#Outlier
#CRYPTO19.DT <- CRYPTO19.DT[V3 != 'HIVE']

# File 2: Descriptive information of each cryptocurrency (source in JSON format)
# Id: Index identifier
# Url, Name, Symbol, Full name, Algorithm, Prooftype,...
library(rjson)
CoinCharact19.lst <- fromJSON(file = 'Dataset/CoinCharactFINAL2.json')

#Transform from List to Dataframe
library(plyr)
CoinCharact19.df <- ldply(CoinCharact19.lst, data.frame)
temp <- merge(CRYPTO19.DT, CoinCharact19.df, by.x = 'FROMSYMBOL', by.y = 'Symbol',
              all.x = TRUE)
CRYPTO19.DT <- as.data.table(temp[,c('FROMSYMBOL','Id','MKTCAP', 'SUPPLY','TotalCoinSupply','Url',
                                     'Algorithm','ProofType')])
CRYPTO19.DT$FROMSYMBOL <- as.character(CRYPTO19.DT$FROMSYMBOL)
CRYPTO19.DT$Id <- as.character(CRYPTO19.DT$Id)
CRYPTO19.DT$SUPPLY <- as.numeric(CRYPTO19.DT$SUPPLY)
CRYPTO19.DT$TotalCoinSupply <- as.numeric(CRYPTO19.DT$TotalCoinSupply)
CRYPTO19.DT$Url <- as.character(CRYPTO19.DT$Url)

CRYPTO19.DT.backup <- CRYPTO19.DT

# Market cap computed as median value of Marketcap (VOLUMETO) variable for each cryptocurrency
# Incluimos el Marketcap mediano por criptomoneda durante 2018 (en lugar de median para evitar outliers)
temp <- CRYP.RT.short[,.(median(volume)), by = SYM]
CRYPTO19.DT <- merge(CRYPTO19.DT,temp, all.y = TRUE, by.x = 'FROMSYMBOL', by.y = 'SYM' )
setnames(CRYPTO19.DT,'V1','volume')



#############################################################################
#             END OF CRYPTOCURRENCIES TREATMENT                             #
#############################################################################

#################################################################################
#                         CCI30 INDEX TREATMENT                                 #
#################################################################################
# Data Transformation of time-series to use Portfolio library
CCI30Index <- read.table("Dataset/cci30_OHLCV.csv",header=TRUE, sep=",")
CCI30Index_zoo <- read.csv.zoo('Dataset/cci30_OHLCV.csv', header=TRUE, sep=',', format='%Y-%m-%d')
CCI30Index_xts <- as.xts(CCI30Index_zoo)

library('PerformanceAnalytics')
CCI30Index_xts$RtSP <- Return.calculate(CCI30Index_xts$Close)

CCI30Index$Date <- as.Date(CCI30Index$Date)
CCI30Index.DT <- as.data.table(CCI30Index)
CCI30Index.DT <- CCI30Index.DT[order(-rank(Date))]


# Transform dayly returns to continous dayly returns transforming by Log (ln) function
CCI30Index.DT <- CCI30Index.DT[,.(Date, Open, High, Low, Close, Volume,
                                  rendCCI30 = log(Close)-log(Close[-1]),
                                  RtCCI30 = (Close - Close[-1])/Close[-1])]


setnames(CCI30Index.DT,'Date','time')

### End of CCI30 Index treatment ################################################

########### Fixed rate investment instruments ############################
############## U.S. Treasury Bill 90 days #####################################
Letra90.DS <- read.table("Dataset/TreasuryBill90.csv",header=TRUE, sep=";")
Letra90.DT <- as.data.table(Letra90.DS)
names(Letra90.DT) <- c('Date', 'X90dayTreasuryBill')
Letra90.DT$Date <- as.Date(Letra90.DT$Date, tryFormats=c("%m/%d/%Y"))


######## VARIABLES DE USO POR OTROS SCRIPTS #####################################
# Salvamos las principales variables de los diferentes datasets
CRYP.RT <- CRYP.RT.short

# Dataset 2018
nfich1 <- c("Dataset/CRYPT_DS_18Rev1.RData")
#save(CRYPrice_xts, CRYP.DT.Filtered, CRYP.RT, CRYP.LST, CRYP.timeWise.rend, 
#       CRYP.timeWise.price, file=nfich1)
#load(nfich1)

# Dataset de Criptomonedas ########################################################
nfich2 <- c("Dataset/CoinsDescr19_18Rev1.RData")
#save(CRYPTO19.DT, file=nfich2)
#load(nfich2)

# Dataset de Crypto Index ########################################################
nfich3 <- c("Dataset/CCI30IndexRev1.RData")
#save(CCI30Index.DT, CCI30Index_xts, file=nfich3)

# Dataset Indices de references ########################################################
nfich4 <- c("Dataset/IndexesRef.RData")
#save(CCI30Index.DT, CCI30Index_xts, Letra90.DS, Letra90.DT, file=nfich4)

nfich5 <- c("Dataset/MyPLDIndex.RData")
#save(PLD, PLD.HQ, file=nfich)
#load(nfich)
