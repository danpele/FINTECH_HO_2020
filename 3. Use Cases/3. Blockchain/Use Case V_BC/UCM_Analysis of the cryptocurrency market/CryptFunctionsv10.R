#### MATH FUNCTIONS #######
#Creation of a list of cryptocurrencies with  daily quotation, returns and marketcap
# <funcCrypto> return a list with following objects:
#1) Dataframe1: CRYPT ticket, first and last day of quotation, traded days
#2) Dataframe2: time-series of quotation (close, timestamp)
#3) Dataframe3: time-series of marketcap (marketcap, timestamp)
#4) Dataframe4: time-sries of daily returns
funCrypto <- function(n1){
  nz <- CRYP.wDF.close[,colnames(CRYP.wDF.close)[n1]]
  nz2 <- CRYP.wDF.volume[,colnames(CRYP.wDF.volume)[n1]]
  nz3 <- CRYP.wDF.rend[,colnames(CRYP.wDF.rend)[n1]]
  
  rg.fech <- CRYP.wDF.close$time[!is.na(nz)]
  first_fech <- rg.fech[1]
  last_fech <- tail(rg.fech, n=1)
  
  basicInf.df <- data.frame(colnames(CRYP.wDF.close)[n1], first_fech, last_fech, length(rg.fech))
  colnames(basicInf.df) <- c('CRYP','First','Last','Samples')
  serieClose.df <- cbind.data.frame(rg.fech,na.omit(nz))
  colnames(serieClose.df) <- c('Fecha','Close')
  serieVolume.df <- cbind.data.frame(rg.fech,na.omit(nz2))
  colnames(serieVolume.df) <- c('Fecha','Volume')
  serieRend.df <- cbind.data.frame(rg.fech,na.omit(nz3))
  colnames(serieRend.df) <- c('Fecha','Rend')
  result <- list(Crypt=colnames(CRYP.wDF.close)[n1], BasicInfo=basicInf.df,
                 serieclose=serieClose.df, serieVolume=serieVolume.df, serieRend=serieRend.df)        
  
  return(result)
}
#Quantile categories
quantFunc <- function(n,q){
  if(is.na(n)) return ('NA')
  if(n<q[2]) return ('Low')
  if(n>q[5]) return ('High')
  return ('Average')
}

#Deciles categories
decFunc <- function(n,p){
  if(is.na(n)) return ('NA')
  if(n<=p[4]) return ('D4')
  if(n<=p[5] & n>p[4]) return ('D5')
  if(n<=p[6] & n>p[5]) return ('D6')
  if(n<=p[7] & n>p[6]) return ('D7')
  if(n<=p[8] & n>p[7]) return ('D8')
  if(n<=p[9] & n>p[8]) return ('D9')
  return ('D10')
}

#Percentile categories
percFunc <- function(n,p){
  if(is.na(n)) return ('NA')
  if(n<=p[70]) return ('P70')
  if(n<=p[80] & n>p[70]) return ('P80')
  if(n<=p[90] & n>p[80]) return ('P90')
  if(n<=p[99] & n>p[90]) return ('P99')
  return ('P100')
}

#Specific Beta categories
betaFunc <- function(b){
  if(is.na(b)) return ('NA')
  if(b< -0.01) return ('NegBeta') #Inverse relation with the market Negative Beta
  if((b>= -0.01) && (b<0.01)) return ('CashLike') #cash like
  if((b>= 0.01) && (b<0.95)) return ('LowVol') #Volatilidad Lower than market
  if((b>= 0.95) && (b<1.05)) return ('Indexlike')
  if((b>= 1.05) && (b<100)) return ('HighVol') #High volatility -Techno like-
  if(b> 100) return('Extreme') #Extreme price swing
}

#Specific Sharp categories
sharpeFunc <- function(s){
  if(is.na(s)) return ('NA')
  if(s<0) return('SRF') #Smaller Risk-free
  if(s>=0.0 && s< 0.5) return('ERP') #Excess return positive
  if(s>=0.5 && s<1.0) return ('Acc') #Acceptable for investment but risky
  if(s>=1.0)  return ('GOOD') #Aceptable to Good by investor
}


## Sigmoide ##
sigmoid <- function(x,a=0){
  return(1/(1+exp(-(x-a))))
}

Norm <- function(x){
  return((x-mean(x))/sd(x))
}

##### FEATURE QUALIFICATION
#Qualy1: sign of the step
QSignFunc <- function(n){
  if(is.na(n)) return ('NA')
  if(n<0) return (-1)
  return (+1)
}

QSignFunc2 <- function(n,p){
  if(is.na(n)) return ('NA')
  if(n<=p[2]) return (-1)
  if(n>p[2] & n<=p[3]) return (0)
  return (+1)
}

#Qualy2: absolute value of the step
#We qualify the absolut value of returns (ccrend): 4 levels
#Quantile categories
QStepFunc <- function(n,p){
  if(is.na(n)) return ('NA')
  if(n<=p[2]) return (0)
  if(n>p[2] & n<=p[3]) return (1)
  if(n>p[3] & n<=p[4]) return (2)
  return (3)
}

#Deciles: absolute value of the step
#We qualify the absolut value of returns: 9 levels
#Quantile categories
QDecFunc <- function(n,p){
  if(is.na(n)) return ('NA')
  if(n<=p[1]) return (0)
  if(n>p[2] & n<=p[3]) return (1)
  if(n>p[3] & n<=p[4]) return (2)
  if(n>p[4] & n<=p[5]) return (3)
  if(n>p[5] & n<=p[6]) return (4)
  if(n>p[6] & n<=p[7]) return (5)
  if(n>p[7] & n<=p[8]) return (6)
  if(n>p[8] & n<=p[9]) return (7)
  return (8)
}

##the normalization function is created
nor <-function(x) { (x -min(x))/(max(x)-min(x))   }

##Confussion matrix qualifications
accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}
PPV <- function(x){x[4]/(x[4]+x[2])} #Positive Predictive Value PPv=TP/(TP+FP)
Pgain<-function(x){(x[3]+x[4])/sum(x)} #Gain probability = (TP+FN)/(TP+FP+TN+FN)=(TP+FN)/N

##Return table computation (Continous Compound Multi-Period Interest Return)
##Sequence:
## We purchase a crypto if we predict is going up tomorrow
## We hold a crypto of we predict that is 
CCIRFun <- function(result.mod){ #result.mod is a data.frame (Date, Ticker, Rt, ccReturn, PRED, TEST,..)
  result.mod$PRED <- as.character(result.mod$PRED)
  result.mod$TEST <- as.character(result.mod$TEST)
  aaa <- (result.mod$PRED=='1')
  result.mod$ccReturnBuy <- c(0)
  result.mod$RtBuy <- c(0)
  result.mod[aaa,]$ccReturnBuy <- result.mod[aaa,]$ccReturn
  result.mod[aaa,]$RtBuy <- result.mod[aaa,]$Rt
  #result.mod <- result.mod[result.mod$ccReturnBuy!=0,]
  result.mod$acumccReturn <- ave(result.mod$ccReturnBuy, result.mod$Date, FUN = cumsum)
  return (result.mod)
}

createTimeSlices <- function(y, initialWindow, horizon = 1, fixedWindow = TRUE, skip = 0) {
  ## initialwindow = initial number of consecutive values in each training set sample
  ## horizon = number of consecutive values in test set sample
  ## fixedwindow = FALSE if we use the maximum possible length for the training set
  ## Ensure that initialwindow + horizon <= length(y)
  
  #stops <- seq(initialWindow, (length(y) - horizon), by = skip + 1)
  stops <- seq(initialWindow, (nrow(y) - horizon), by = skip + 1)
  
  if (fixedWindow) {
    starts <- stops - initialWindow + 1
  } else {
    starts <- rep(1, length(stops)) # all start at 1
  }
  
  train <- mapply(seq, starts, stops, SIMPLIFY = FALSE)
  test <- mapply(seq, stops+1, stops+horizon, SIMPLIFY = FALSE)
  nums <- gsub(" ", "0", format(stops))
  names(train) <- paste("Training", nums, sep = "")
  names(test) <- paste("Testing", nums, sep = "")
  
  out <- list(train = train, test = test)
  
  return(out)
}

#Funtion for detailed analysis of the association for the different values of categoriacal variables
residPearson.mtx <- function(chiQres.mtx, chiQobs.mtx){ #The parameter are the residuals of the contigency table
  maxcol <- max.col(abs(chiQres.mtx)) #We filter by the maxium residual values
  nr <- dim(chiQres.mtx)[1]
  i <- 1:nr
  #techno <- data.frame(Alg=character(), Cons=character(), res=numeric(), stringsAsFactors = FALSE)
  temp.res <- vapply(1:nr,function(x) {chiQres.mtx[x,maxcol[x]]}, numeric(1))
  temp.obs<- vapply(1:nr,function(x) {chiQobs.mtx[x,maxcol[x]]}, numeric(1))
  techno <-cbind(rownames(chiQres.mtx)[i], colnames(chiQres.mtx)[maxcol[i]], temp.res, temp.obs)        
  colnames(techno) <- c('Var1','Var2','res','obs')
  row.names(techno)<-c()
  
  techno <- as.data.frame(techno, stringsAsFactors = FALSE)
  techno$res <- as.numeric(techno$res)
  techno$obs <- as.integer(techno$obs)
  techno <- techno[order(techno$res, decreasing = TRUE),]
  techno <- techno[order(-abs(techno$res)),] #Dataframe ordenado por residuo
  return(techno)
} 

#Function to compute the bascket value (Portfolio) with fKelly criterion
MyPortK <- function(Basket,budgInic=100000,Rf=0L,fr=0L){#Rf: return free-risk; fr: market friction
  Basket$Rt <- Basket$Rt - Rf #We compute the exceed rate return
  Equity <- budgInic
  Days <- table(Basket$Date)
  Days <- data.frame(Days, stringsAsFactors = FALSE)
  names(Days)<-c('Date','Freq')
  Days$Date <- as.Date(Days$Date)
  
  MyBudg <- data.frame()
  for(day in Days$Date){
    #print(as.Date(day))
    OneDay.df <- subset(Basket,Date==day, select = c(1,2,3,4,11)) #Date, Ticker, PriceYest, Rt, fKelly1Y
    OneDay.df$Ticker<-as.character(OneDay.df$Ticker)
    longPos.day<-data.frame()
    shortPos.day<-data.frame()
    Equity.day <- Equity
    MyBudg.day <- data.frame()
    for(i in 1:nrow(OneDay.df)){
      obs <- OneDay.df[i,]
      #print(i)
      nc <- floor(obs$fKelly1Y*Equity.day/obs$PriceYest) #number of the cryptocoin i bought at the end of the day on t-1
      if (nc==0){next}
      longPos <- nc*obs$PriceYest
      shortPos <- (obs$Rt + 1) * longPos
      Rt <- (shortPos-longPos)/longPos
      Equity.day <- Equity.day - longPos
      longPos.day <- rbind(longPos.day,data.frame(obs$Ticker,longPos)) #Cost of the investment
      shortPos.day <- rbind(shortPos.day,data.frame(obs$Ticker,shortPos)) #Refund of investment at the end of the day
      MyBudg.day <- rbind(MyBudg.day, data.frame(obs$Date, obs$Ticker,nc,longPos,shortPos,Rt))
    }
    fees <- fr*(sum(longPos.day[2]) + sum(shortPos.day[2])) #Fee charged per day
    #MyBudg.day$Reff <- (sum(shortPos.day[2])-sum(longPos.day[2]-fees))/sum(shortPos.day[2])
    #W <- MyBudg.day$longPos / sum(longPos.day[2]) #W is a vector with all the weights
    W <- MyBudg.day$longPos / Equity #W is a vector with all the weights
    Reff <- (sum(shortPos.day[2])-sum(longPos.day[2])-fees)/sum(longPos.day[2])
    MyBudg.day$Rt <- W*(MyBudg.day$Rt) #We compute the weight nominal exceed risk-free interest rate
    Equity <- Equity - sum(longPos.day[2]) + sum(shortPos.day[2]) - fees #We refund at the end of the day every day (we sell the portfolio)
    MyBudg <- rbind(MyBudg,data.frame(MyBudg.day, Reff, Equity))
  }
  #names(MyEquity)<-c('Date','EquityEoD')
  #portf <- merge(MyBudg,MyEquity, all.x = TRUE,by.x='obs.Date',by.y = 'Date')
  names(MyBudg)<-c('Date','Ticker','Pos','LongPos','SortPos','WERt','Reff','Equity')
  return(MyBudg)
}

#Function to compute the bascket value (Portfolio) with Naive criterion
MyPortN <- function(Basket,budgInic=100000L,Rf=0L,fr=0L){
  Basket$Rt <- Basket$Rt - Rf #We compute the exceed rate return (Market Risk Premium)
  Equity <- budgInic
  Days <- table(Basket$Date)
  Days <- data.frame(Days, stringsAsFactors = FALSE)
  names(Days)<-c('Date','Freq')
  Days$Date <- as.Date(Days$Date)
  
  MyBudg <- data.frame()
  for(day in Days$Date){
    OneDay.df <- subset(Basket,Date==day, select = c(1,2,3,4)) #Date, Ticker, PriceYest, Rt
    OneDay.df$Ticker<-as.character(OneDay.df$Ticker)
    N <- length(unique(OneDay.df$Ticker))
    budgN <- Equity/N #With Naive criterion, we allocate same budget for each cryptocoin
    W <- 1/N #Weight of each cryptocoin
    longPos.day<-c(0)
    shortPos.day<-c(0)
    Equity.day <- Equity
    MyBudg.day<-data.frame()
    for(i in 1:nrow(OneDay.df)){
      obs <- OneDay.df[i,]
      nc <- floor(W*Equity.day/obs$PriceYest) #number of the cryptocoin i bought at the end of the day on t-1
      if (nc==0){next}
      longPos <- nc*obs$PriceYest
      longPos.day <- longPos.day + longPos #Cost of the investment
      #Equity.day <- Equity.day - longPos
      shortPos <- (obs$Rt + 1) * longPos #Refund when we sell at the end of the day
      shortPos.day <- shortPos.day +  shortPos #Accumulated refund at the end of the day
      #Rt <- W*(shortPos-longPos)/longPos #We weight the returns
      Rt <- W*obs$Rt
      MyBudg.day <- rbind(MyBudg.day, data.frame(obs$Date, obs$Ticker,nc,longPos,shortPos, Rt))
    }
    fees <- fr*(longPos.day+shortPos.day) #Fee charged per day
    Equity <- Equity - longPos.day + shortPos.day - fees #We rebalance at the end of the day
    #MyBudg.day$Reff <- (shortPos.day-longPos.day-fees)/shortPos.day
    Reff <- (shortPos.day-longPos.day-fees)/longPos.day
    MyBudg <- rbind(MyBudg,data.frame(MyBudg.day, Reff, Equity))
  }
  names(MyBudg)<-c('Date','Ticker','Pos','LongPos','SortPos','WERt','Reff','Equity')
  return(MyBudg)
}

#Function to compute the bascket value (Portfolio) with Index criterion (unlimited budget)
MyPortI <- function(Basket,Rf=0L,fr=0L){
  Basket$Rt <- Basket$Rt - Rf #We compute the exceed rate return
  #Equity <- budgInic
  Days <- table(Basket$Date)
  Days <- data.frame(Days, stringsAsFactors = FALSE)
  names(Days)<-c('Date','Freq')
  Days$Date <- as.Date(Days$Date)
  
  MyBudg <- data.frame()
  for(day in Days$Date){
    OneDay.df <- subset(Basket,Date==day, select = c(1,2,3,4)) #Date, Ticker, PriceYest, Rt
    OneDay.df$Ticker<-as.character(OneDay.df$Ticker)
    #N <- length(unique(OneDay.df$Ticker))
    #budgN <- Equity/N #With Naive criterion, we allocate same budget for each cryptocoin
    #W <- 1/N #Weight of each cryptocoin
    longPos.day<-c(0)
    shortPos.day<-c(0)
    Equity.day <- sum(OneDay.df$PriceYest)
    MyBudg.day <- data.frame()
    for(i in 1:nrow(OneDay.df)){
      obs <- OneDay.df[i,]
      #nc <- floor(W*Equity.day/obs$PriceYest) #number of the cryptocoin i bought at the end of the day on t-1
      nc <- c(1) #We bought one of each cryptocoin at the beginning of the day 
      #if (nc==0){next}
      longPos <- nc*obs$PriceYest
      longPos.day <- longPos.day + longPos #Cost of the investment
      #Equity.day <- Equity.day - longPos
      shortPos <- (obs$Rt + 1) * longPos
      shortPos.day <- shortPos.day +  shortPos #Refund of investment at the end of the day
      W <- obs$PriceYest / Equity.day
      #Rt <- W*(shortPos-longPos)/longPos #We weight the returns
      Rt <- W*obs$Rt
      MyBudg.day <- rbind(MyBudg.day, data.frame(obs$Date, obs$Ticker,nc,longPos,shortPos, Rt))
    }
    fees <- fr*(longPos.day+shortPos.day) #Fee charged per day
    #MyBudg.day$Reff <- (shortPos.day-longPos.day-fees)/shortPos.day
    Reff <- (shortPos.day-longPos.day-fees)/longPos.day
    DiffEquity <- Equity.day-shortPos.day-fees
    MyBudg <- rbind(MyBudg,data.frame(MyBudg.day, Reff, DiffEquity))
  }
  names(MyBudg)<-c('Date','Ticker','Pos','LongPos','SortPos','WERt','Reff','DiffEquity') #WERt: Weight Exceed Return
  return(MyBudg)
}

AggPortfRet <- function(portf){  # Returns of the market
  portf.dt <- as.data.table(portf)
  setkey(portf.dt,Date)
  AggRet <- portf.dt[,.(Rd=sum(WERt)),by=Date]
  AggRet$Reff <- portf.dt[J(unique(Date)),mult='first']$Reff
  names(AggRet)<-c('Date','SumWdER','Reff') #Sum of weight daily Exceed Return
  return(AggRet)
}
