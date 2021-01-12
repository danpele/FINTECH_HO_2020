#####################################################################
##### FASE 2: QUANTITATIVE CLUSTERS ANALYSIS                    #####
##### We condier following techniques:
##### 1) Kmeans on the scalated mean returns and volatility 
##### 2) Hist DAWass on the ordinarly daily log-returns
##### 3) TADPole on the ordinary daily log-returns
##### 4) Intersection of clusters
#####################################################################
#Source: ClusterAnalysisV31g3.R

# Libraries

library(reshape2)
library(dplyr)
library(data.table)
library(xtable)

#Datasets for CLUSTERING: Tables of 2018 computed by DataSet2018_Cryptooins.R script
#SOURCE TABLES: CRYPrice_xts, CRYP.DT.Filtered, CRYP.RT, CRYP.LST, CRYP.timeWise.rend, 
setwd('D:/DOCTORADO/Scripts/R/Revision1/')

source('CryptFunctionsV10.R')

nfich1 <- c("Dataset/CRYPT_DS_18Rev1.RData")
load(nfich1)

# Dataset de Criptomonedas ########################################################
nfich2 <- c("Dataset/CoinsDescr19_18Rev1.RData")
load(nfich2)

# Dataset Indices de references ########################################################
nfich4 <- c("Dataset/IndexesRef.RData")
load(nfich4)

nfich5 <- c("Dataset/MyPLDIndex.RData")
load(nfich5)

CRYP.RT[CRYP.RT$SYM=='BTC',]

# We define some auxiliar tables ##########################################################
CRYP_dep.RT <- CRYP.RT #630618
CRYPrice_dep_xts <- CRYPrice_xts

# We make the Table for Betas computation
BetaTabla <- merge.data.frame(CRYP_dep.RT, CCI30Index.DT, by ='time', 
                              all.x = TRUE)
#Cleaning of Beta table and format change:
BetaTabla <- na.omit(BetaTabla) #630618
BetaTabla.DT <- as.data.table(BetaTabla)
BetaTabla.DT <- BetaTabla.DT[,.(time, SYM, Rt, RtCCI30)]
setnames(BetaTabla.DT,c('Rt','RtCCI30'), c('Rt.crypt','Rt.CCI30'))
BetaTabla.DT$SYM <- as.factor(BetaTabla.DT$SYM)



########## TABLE: Crypto.Qualy ##################################
# We make a quality table collecting all categoric information  #
#################################################################

# Categories for volume
MKCaProxy.Perctil <- quantile(CRYPTO19.DT$volume, probs = seq(0,1,length = 101),type = 8)
QMKCaProxy <- CRYPTO19.DT[,.(PercMKCaProxy = percFunc(volume, MKCaProxy.Perctil)),
                      by = FROMSYMBOL]

############### Crypto.Qualy DF #######################
Crypto.Qualy <- merge(CRYPTO19.DT, QMKCaProxy, all.x = TRUE)
Crypto.Qualy <- Crypto.Qualy[,.(FROMSYMBOL,Algorithm,ProofType,PercMKCaProxy)]
names(Crypto.Qualy)<- c('SYM','Algorithm','ProofType','PercMKCapProxy')
#######################################################


MCTot <- sum(CRYPTO19.DT$volume) #1162399644
MKTCAPShare <- CRYPTO19.DT[,.(FROMSYMBOL,MKCapRate = volume/MCTot)]
MKTCAPShare <- MKTCAPShare[order(MKTCAPShare$MKCapRate,decreasing = TRUE),]

MKCapAcum <- double()
for (i in 1:dim(MKTCAPShare)[1]){
        MKCapAcum <- append(MKCapAcum, sum(MKTCAPShare$MKCapRate[1:i]))
}

MKTCAPShare$acumul <- MKCapAcum
MKTCAPShare$ClaseMKCaProxy <- c('BC') #Marcamos cryptom de baja capitalizacion
temp <- MKTCAPShare[,.(MKCapRate >= 0.01)]
MKTCAPShare$ClaseMKCaProxy[temp$V1]=c('AC') #Criptomonedas de alta capitalizacion (1%)
colnames(MKTCAPShare)[1] <- 'SYM'
MKTCAPShare$ClaseMKCaProxy <- as.factor(MKTCAPShare$ClaseMKCaProxy)
nrow(MKTCAPShare)
### Ordered Market cap list #############
write.table(MKTCAPShare, file = "Tables/MKCaProxy.txt", sep = "\t",
            row.names = TRUE, col.names = NA)


#CRYP.RT[CRYP.RT$SYM=='BTC',]

### High Liquidity ranking: The Top-ten
crypt.cap <- MKTCAPShare[1:20,.(SYM)]
crypt.cap

################# Crypto.Qualy DF ################################
Crypto.Qualy <- merge(Crypto.Qualy,MKTCAPShare)
Crypto.Qualy <- Crypto.Qualy[,.(SYM,Algorithm, ProofType,PercMKCapProxy,ClaseMKCaProxy)]
Crypto.Qualy$SYM <- as.character(Crypto.Qualy$SYM)

##################### NEW #################################
# MARKETCAP ###############################################
###########################################################
# Categories for Marketcap
MKCaP.Perctil <- quantile(CRYPTO19.DT$MKTCAP, probs = seq(0,1,length = 101),type = 8)
QMKCaP <- CRYPTO19.DT[,.(PercMKCaP = percFunc(MKTCAP, MKCaP.Perctil)),
                          by = FROMSYMBOL]

############### Crypto.Qualy DF ##############################################
Crypto.Qualy <- merge(Crypto.Qualy, QMKCaP, all.x = TRUE, by.x = 'SYM', by.y = 'FROMSYMBOL')
##############################################################################

MCTot.real <- sum(CRYPTO19.DT$MKTCAP) #2.70531e+11
MKTCAPShare.real <- CRYPTO19.DT[,.(FROMSYMBOL,MKCapRate.real = MKTCAP/MCTot.real)]
MKTCAPShare.real <- MKTCAPShare.real[order(MKTCAPShare.real$MKCapRate.real,decreasing = TRUE),]

MKCapAcum.real <- double()
for (i in 1:dim(MKTCAPShare)[1]){
    MKCapAcum.real <- append(MKCapAcum.real, sum(MKTCAPShare.real$MKCapRate.real[1:i]))
}

MKTCAPShare.real$acumul.real <- MKCapAcum.real
MKTCAPShare.real$ClaseMKCaP <- c('BC') #Marcamos cryptom de baja capitalizacion
temp <- MKTCAPShare.real[,.(MKCapRate.real >= 0.01)]
MKTCAPShare.real$ClaseMKCaP[temp$V1]=c('AC') #Criptomonedas de alta capitalizacion (1%)
colnames(MKTCAPShare.real)[1] <- 'SYM'
MKTCAPShare.real$ClaseMKCaP <- as.factor(MKTCAPShare.real$ClaseMKCaP)
nrow(MKTCAPShare.real)
### Ordered Market cap list #############
write.table(MKTCAPShare.real, file = "Tables/MKCaP.txt", sep = "\t",
            row.names = TRUE, col.names = NA)


#CRYP.RT[CRYP.RT$SYM=='BTC',]

### Market cap ranking: The Top-ten
#crypt.cap <- MKTCAPShare.real[1:20,.(SYM)]
#crypt.cap

################# Crypto.Qualy DF ###################################
Crypto.Qualy <- merge(Crypto.Qualy,MKTCAPShare.real)
Crypto.Qualy <- Crypto.Qualy[,.(SYM,Algorithm, ProofType,PercMKCapProxy,ClaseMKCaProxy, PercMKCaP, ClaseMKCaP)]
#####################################################################

########## TABLE: EUrendvol #######################################
# We make a pure quantitative table aggreagting by yearly basis   #
# vmedio: mean value of volume in 2018
# vmin: minimum value of volume variable 
# vmax: maximum value of volume variable
# v90: Percentile P90 of volume distribution (same for v75, v25 and v10)
# rmedio: mean value of log-return (continous compound daily return) rend in 2018
# rmin: minimum value of rend variable
# rmax: maximum value of rend variable
# r90: Percentile P90 of rend distribution (same for v75, v25 and v10)
# rmediano: median log-return in 2018
# iqr: Interquartile of rend variable
# volatility: standard deviation of red variable
# n: traded days (usually 366 days)
###################################################################

EUrendvol <- CRYP_dep.RT[,.(vmedio = mean(volumeUSD), vmin = min(volumeUSD), vmax=max(volumeUSD), 
                            v90 = quantile(volumeUSD,probs = c(0.90)), v75 = quantile(volumeUSD,probs = c(0.75)), 
                            v25 = quantile(volumeUSD,probs = c(0.25) ), v10 = quantile(volumeUSD,probs = c(0.1)), 
                            rmedio = mean(rend), rmin = min(rend), rmax=max(rend), r90 = quantile(rend,probs = c(0.90)), 
                            r75 = quantile(rend,probs = c(0.75)), r25 = quantile(rend,probs = c(0.25) ), 
                            r10 = quantile(rend,probs = c(0.1)), rmediano = median(rend), iqr = IQR(rend), 
                            volatility = sd(rend), n=.N), 
                         by = SYM]

VolumSumm<- summary(EUrendvol$vmedio)
RendSumm <- summary(EUrendvol$rmedio)
VolatSumm<- summary(EUrendvol$volatility)

# Descriptive statistics of Volume, Returns and Volatility
VolumSumm
RendSumm
VolatSumm

# Clasificamos las criptomonedas por niveles B, M y A de rendimientos, volumen y volat
# Clasificacion por Cuantiles:

# Qualification of quantitative variable: variable transformation from quantitative to categorical
QRendVol <- EUrendvol[,.(ClaseVolu = quantFunc(vmedio, VolumSumm),
                         ClaseRend = quantFunc(rmedio, RendSumm),
                         ClaseVola = quantFunc(volatility, VolatSumm)),
                      by = SYM]

EUrendvol <- merge(EUrendvol,QRendVol,by = 'SYM', all.x = TRUE)

################# Crypto.Qualy DF ############################
Crypto.Qualy <- merge(Crypto.Qualy,QRendVol, by = 'SYM')
##############################################################

Volum.Dec <- quantile(EUrendvol$vmedio, probs = seq(0,1,length = 11),type = 5)
Rend.Dec <- quantile(EUrendvol$rmedio, probs = seq(0,1,length = 11),type = 5)
Volat.Dec <- quantile(EUrendvol$volatility, probs = seq(0,1,length = 11),type = 5)
DRendVol <- EUrendvol[,.(DecVolu = decFunc(vmedio, Volum.Dec),
                         DecRend = decFunc(rmedio, Rend.Dec),
                         DecVola = decFunc(volatility, Volat.Dec)),
                      by = SYM]

########### Crypto.Qualy DF #################################
Crypto.Qualy <- merge(Crypto.Qualy,DRendVol)
#############################################################

Crypto.Qualy[Crypto.Qualy$SYM=='BTC',]

################################### TABLE: EUrisk #########################################
# We make Risk-table based on well stated standard risk measures, we apply some functions of
# PerformanceAnalytics package, very helfull package for portoflio managers
###########################################################################################
library(PerformanceAnalytics)

EUrisk <- CRYP_dep.RT[,.(VaR01 = VaR(rend, method = 'historical', p=.99, invert = TRUE)[1],
                         CVaR01 = ETL(rend, p=.99, method = 'historical',invert = TRUE)[1],
                         VaR05 = VaR(rend, method = 'historical' ,p = .95,invert = TRUE)[1],
                         CVaR05 = ETL(rend, p=.95, method = 'historical', invert = TRUE)[1],
                         rmedio = mean(rend), volatility = sd(rend), 
                         cv = sd(rend)/mean(rend),
                         skew=skewness(rend), kurt=kurtosis(rend),
                         n=.N
), by = SYM]

EUrisk[EUrisk$SYM=='BTC',]
#SYM      VaR01     CVaR01       VaR05     CVaR05       rmedio volatility        cv       skew     kurt   n
#1: BTC -0.1188108 -0.1582526 -0.07920107 -0.1128083 -0.003298326  0.0441322 -13.38018 -0.4390315 1.897822 366

###########################################################################
##### Computation of some  FINANCE RATIOS #################################
### Sharpe, Sortino, Modigliani-Modigliani
###########################################################################

#Sharpe ratio (vs Sortino ratio)
#rf <- mean(Letra90.DT$X90dayTreasuryBill/100) 
#Continous Compound Daily Risk Free interest (Annualized over90 days U.S T-Bill)
rf <- mean(1+Letra90.DT$X90dayTreasuryBill/100)**(1/365) - 1 #5.254377e-05

CRYPRend_xts <- t(CRYP.timeWise.rend)
SharpeR <- SharpeRatio(CRYPRend_xts, Rf=rf, FUN = 'StdDev')
EUrisk$SharpeR <- SharpeR[1,]
DSharpeR <- sapply(EUrisk$SharpeR, function(s) sharpeFunc(s))

##################### Crypto.Qualy DF ####################################
Crypto.Qualy$DecSharpeR <- DSharpeR
##########################################################################

##### Beta processing #################################################
## Beta computed taken CCI30 index           #
#######################################################################
Betas <- BetaTabla.DT[,.(betaCryp=coef(lm(Rt.crypt ~ Rt.CCI30))[2]), by = SYM]
DBetas <- Betas[,.(DBeta=betaFunc(betaCryp)), by = SYM] #Categorical treatment

### Crypto.Qualy DF: We incorporate Beta values to Quanty EUrisk and Qualy Crypto.Qualy tables
Crypto.Qualy <- merge(Crypto.Qualy,DBetas)
#########################################################################

EUrisk <- merge(EUrisk,Betas, by='SYM',all.x = TRUE) #Number of cryptocurrencies (True value): 1723 obs

###### Maturity processing #######################################################
# We compute the total number of traded days of each cryptocurrency on the market 
# starting when the cryptocoins start to work on the market
##################################################################################

# First some cleaning
# We remove rergisters with zero values on the variables CLOSE, HIGH and OPEN
DiasCotizados <- CRYP.DT.Filtered[open !=0 & close!=0 & high!=0 & low!=0 & volumefrom!=0]
Edad.cryp <- DiasCotizados[,.N, by = SYM] #Days on the market
Age.Dec <- quantile(Edad.cryp$N, probs = seq(0,1,length = 11),type = 5, na.rm = TRUE)
DecAge <- sapply(Edad.cryp$N, function(s) decFunc(s,Age.Dec)) #We qualify the maturity in categories
Edad.cryp$DAge <- DecAge

#Crypto.Qualy DF: We add maturity to the Qualy table ###########################
Crypto.Qualy <- merge(Crypto.Qualy, Edad.cryp, all.x = TRUE)
Crypto.Qualy <- Crypto.Qualy[,c("N"):=NULL]
################################################################################

#### New Table mixing some Qualy y cuantitative: Crypto.Famd #########################
Crypto.Famd <- data.frame()
Crypto.Famd <- EUrisk[,.(SYM, rmedio, volatility)]
Crypto.Famd$SYM <- as.character(Crypto.Famd$SYM)

########################### CLUSTERING ####################################################

########################################################################
###    TECHNIQUE 1: Kmeans Bi-variate (mean log-return , volatilidad)###
########################################################################
#STEP 1) Definition of the dataframe for the K-means clustering

EUrendvol.red <- as.matrix(EUrendvol[,.(volatility,rmedio)]) #Bi-variate matrix
colnames(EUrendvol.red) <- c('volatility','average_return')
rownames(EUrendvol.red) <- EUrendvol$SYM

df.ord <- EUrendvol.red[,c('volatility','average_return')] #Ordinary values dataframe
df <- scale(EUrendvol.red[,c('volatility','average_return')]) #Scalated values dataframe

#Verification of structure for mean average and volatility plane by Hopkins method
library(clustertend)

set.seed(123)
#sink('D:/DOCTORADO/Scripts/R/Entrega/DataSets/hopkins.txt')
hopkins(df, n = nrow(df)-1)
#sink()

#STEP 2) Selection of the Optimum number of K-means clusters ###########################
library(NbClust)
set.seed(123)
nb_max <- NbClust(df, distance = 'maximum', min.nc = 2, max.nc = 15, method = 'kmeans')

#sink('Tables/NbKmeans.txt')
nb_max
#sink()

#STEP 4) Kmeans over scalated dataframe for the selected number of clusters ##############
set.seed(123)
i <- 1:10 #Number of trials
k3_iter <- lapply(i, function (i) {kmeans(df, centers = 3, nstart = 50)})

#STEP 5) Ensemble of the results for all trials
library(clue)

k3_ensemb <- cl_ensemble(list = k3_iter)
names(k3_ensemb) <- paste0("r_",1L:10L)

#plot(cl_dissimilarity(k3_ensemb, method = 'euclidean'), labels = FALSE, pch=4)

#k3_consensus <- cl_consensus(k3_ensemb)
k3_medoid <- cl_medoid(k3_ensemb) #k3_medoid is the clustering medoid of all K-means iterations

#Cardinality
k3_medoid$size

#sink('Tables/CardKmeans.txt')
k3_medoid$size
#sink()

#STEP 6) GRAPHS
library(ggplot2)
library(FactoMineR)
library(factoextra)
#First K-means clustering representation
# fviz_cluster(k3_medoid, data = df.ord, geom = 'text', labelsize = 7, show.clust.cent = TRUE,stand = FALSE,
#              main='K-means: Volatility-Average return')

#We add Clustering information (cluster label) to the tables EUrendvol, Qualy y Famd

fGrupoKmean3=function(x){ #Function to retrieve Cluster label for each cryptocoin
    return(k3_medoid[[1]][x][[1]])
}

EUrendvol$Kmeans = sapply(EUrendvol$SYM,fGrupoKmean3)

######### Crypto.Qualy DF ####################
Crypto.Qualy$Kmeans <- EUrendvol$Kmeans
Crypto.Famd$Kmeans <- EUrendvol$Kmeans

# We change variable type of SYM, Algorithm, ProofType y ClaseMKCap from Factor to String
Crypto.Qualy$SYM <- as.character(Crypto.Qualy$SYM)
Crypto.Qualy$Algorithm <- as.character(Crypto.Qualy$Algorithm)
Crypto.Qualy$ProofType <- as.character(Crypto.Qualy$ProofType)
Crypto.Qualy$ClaseMKCaProxy <- as.character(Crypto.Qualy$ClaseMKCaProxy)
Crypto.Qualy$ClaseMKCaP <- as.character(Crypto.Qualy$ClaseMKCaP)
#Crypto.Qualy$Kmeans <- as.integer(Crypto.Qualy$Kmeans)

#First k-means representation
fviz_cluster(k3_medoid,data = df.ord, geom = 'text', labelsize = 7, show.clust.cent = TRUE,
             main='K-means: Volatility-Average return', stand = FALSE)

#We choose some cryptocurrencies to represent
crypt.km <- rbind.data.frame(c('BCD','DASH','GNT','LSK', 'QTUM','SC','STRAT','NBT','DROP'),
                                c('ADCN','ITT','LBTC','STAR','YOVI','AMIS','BLX','XIN*','RIPT'),
                                c( 'ARN','BAT','BCN','DGB','GVT','MANA','NAS','XMO','REE'),
                             stringsAsFactors = FALSE)
rownames(crypt.km) <- c('Clust1','Clust2','Clust3')
colnames(crypt.km) <- 1:ncol(crypt.km)

df.cap <- subset(df.ord, rownames(df.ord) %in% as.vector(t(crypt.cap)))
df.red <- subset(df.ord, rownames(df.ord) %in% as.vector(t(crypt.km)))

#Final k-means representation: Figure 19 a) K-means clustering for ordinary variables with 
#the position of highest marketcap in red colours:
p <- fviz_cluster(k3_medoid,data = df.ord, geom = 'point',show.clust.cent = TRUE,
                  main='K-means: Volatility-Mean return',ellipse.alpha=0, stand = FALSE)
p <- fviz_add(p,df.red, labelsize = 3, geom = 'text', color = 'dimgray', pointsize = 1, 
              repel = TRUE, ellipse.alpha=0)

fviz_add(p, df.cap, labelsize = 3, color = 'red',repel = TRUE,stand = FALSE)

## Centroid coordinates
Centroides <- Crypto.Famd[,.(RendMed = mean(rmedio), volat = mean(volatility)), by = Kmeans]
#xtable(Centroides, digits = 4)
Centroides

## Bidimensional figures with plotly library: https://plot.ly/r/2d-histogram-contour/
#https://plotly.com/r/2D-Histogram/
 #https://community.plotly.com/t/2d-histogram-contour-plot-with-scatter-plot-overlay-in-r/3040
library(plotly)
library(magrittr)
library(RSelenium)

# Centroid 1 (antes Centroid 2)
cl1 <- Crypto.Famd[Kmeans==1,.(SYM,volatility,rmedio)]
fig <- plot_ly(x=cl1$volatility, y=cl1$rmedio,showscale = F,autocolorscale=T)
#fig <- fig %>% add_text(text = cl2$SYM)
fig <- fig %>% add_markers(alpha=0.4)
# add_histogram2dcontour(fig, showscale=FALSE, ncontours=25) %>%
#     layout(xaxis = list(range = c(0.5, 3.0)),
#            yaxis = list(range= c(-0.03,0.03)),
#            showlegend=FALSE)
add_histogram2dcontour(fig, showscale=FALSE, ncontours=35) %>%
    layout(xaxis = list(range = c(0.5, 5.0)),
           yaxis = list(range= c(-0.04,0.04)),
           showlegend=FALSE)

# Centroid 1 (detail) (antes Centroid 2)
vectKM1 <- c('ICE','8BIT','STAR', 'ADCN', 'GOOD', 'WAND','ITT','ZCG','PFR','ELTCOIN','FLLW','REX')
#cl.red <- cl[cl$SYM %in% vectKM1,]

temp <- (cl1$SYM %in% vectKM1)
cl1.red <- cl1
cl1.red$SYM[!temp] <- intToUtf8(176) #Point ASCII character (183)

fig <- plot_ly(x=cl1.red$volatility, y=cl1.red$rmedio,showscale = F,autocolorscale=T)
fig <- fig %>% add_text(text = cl1.red$SYM, color=I("black"))
add_histogram2dcontour(fig, showscale=FALSE, ncontours=25) %>%
    layout(xaxis = list(range = c(0.5, 2.5)),
           yaxis = list(range= c(-0.03,0.02)),
           showlegend=FALSE)


# Centroid 2 (antes Centroide 3)
cl2 <- Crypto.Famd[Kmeans==2,.(SYM,volatility,rmedio)]
fig <- plot_ly(x=cl2$volatility, y=cl2$rmedio,showscale = F,autocolorscale=T)
#fig <- fig %>% add_text(textfont = t, text = cl$SYM)
fig <- fig %>% add_markers(alpha=0.4)
add_histogram2dcontour(fig, showscale=FALSE, ncontours=140) %>%
    layout(xaxis = list(range = c(-0.001, 0.5)),
           yaxis = list(range= c(-0.007,0.005)),
           showlegend=FALSE)

# Centroid 2 (detail)
# Cryptocurrencies with the best return-volatility rate
vectKM2.best <- c('BSTY','CYP','OPES','BST','BTTF','NZC','ALEX','MMXVI','ETL')
#cl.best <- Crypto.Qualy[Crypto.Qualy$SYM %in% vectKM3.best,]
#cl.best[,.(SYM,PercMKCap, ClaseMKCap)]

temp <- (cl2$SYM %in% vectKM2.best)
cl2.red <- cl2
cl2.red$SYM[!temp] <- intToUtf8(176) #Point ASCII character (183)

fig <- plot_ly(x=cl2.red$volatility, y=cl2.red$rmedio,showscale = F,autocolorscale=T)
fig <- fig %>% add_text(text = cl2.red$SYM)
add_histogram2dcontour(fig, showscale=FALSE, ncontours=15) %>%
    layout(xaxis = list(range = c(0.02, 0.09)),
           yaxis = list(range= c(0.0015,0.0050)),
           showlegend=FALSE)

# Detalle Centroide 2
#Seleccion de criptomonedas
vectKM2 <- c('BTC','EOS','ETC', 'ETH', 'LTC')
temp <- (cl2$SYM %in% vectKM2)
cl2.red <- cl2
cl2.red$SYM[!temp] <- intToUtf8(176) #Point ASCII character (183)

fig <- plot_ly(x=cl2.red$volatility, y=cl2.red$rmedio,showscale = F,autocolorscale=T)
fig <- fig %>% add_text(text = cl2.red$SYM)
add_histogram2dcontour(fig, showscale=FALSE, ncontours=15) %>%
    layout(xaxis = list(range = c(0.03, 0.09)),
           yaxis = list(range= c(-0.006,-0.001)),
           showlegend=FALSE)



# Centroid 3 (antes Centroid 1)
cl3 <- Crypto.Famd[Kmeans==3,.(SYM,volatility,rmedio)]
fig <- plot_ly(x=cl3$volatility, y=cl3$rmedio,showscale = F,autocolorscale=T)
fig <- fig %>% add_markers(alpha=0.4)

temp <- add_histogram2dcontour(fig, showscale=FALSE, ncontours=25) %>%
    layout(xaxis = list(range = c(0.05, 0.5)),
           yaxis = list(range= c(-0.02,0.0)),
           showlegend=FALSE)
temp
# if (!require("processx")) install.packages("processx")
# orca(temp, file = "CentroidKMClust1D2.eps", width = 600, height = 500)
# 
# temp %>%
#     export(file = "D:/DOCTORADO/Scripts/R/Entrega/Datasets/prueba.svg",
#            selenium = RSelenium::rsDriver(browser = "firefox"))
# 
# png('prueba.png', res = 300)
# temp
# dev.off()

# Centroide 3 (detail) (antes Cluster 1)
#Seleccion de criptomonedas
vectKM3 <- c('XEM','VIA','QRL', 'DASH', 'QTUM', 'BCH','XST')
#cl.red <- cl[cl$SYM %in% vectKM1,]

temp <- (cl3$SYM %in% vectKM3)
cl3.red <- cl3
cl3.red$SYM[!temp] <- intToUtf8(176) #Point ASCII character (183)

fig <- plot_ly(x=cl3.red$volatility, y=cl3.red$rmedio,showscale = F,autocolorscale=T)
fig <- fig %>% add_text(text = cl3.red$SYM, color=I("grey"))
add_histogram2dcontour(fig, showscale=FALSE, ncontours=15) %>%
    layout(xaxis = list(range = c(0.06, 0.09)),
           yaxis = list(range= c(-0.009,-0.006)),
           showlegend=FALSE)




#Total Squared Summ for each cluster
k3_medoid$totss

opar <- par(no.readonly = TRUE)
#par(mfcol=c(2,1))

coleccion <- k3_medoid[7]$size
lbls <- seq(1,length(coleccion))

dev.off()
barplot(coleccion,
        main = 'Number of cryptocurrencies by cluster in K-Means',
        xlab = 'Cluster',
        ylab = 'Frequency')
pct <- round(coleccion/sum(coleccion)*100)
lbls2 <- paste('C', lbls, ' ', pct, '%', sep = '') 
pie(k3_medoid[7]$size,
    labels = lbls2, col = rainbow(length(lbls2)),
    radius = 2,cex.lab=0.4,
    main = 'Cryptocurrencies by cluster (%): k=3 ')

par(opar)

#Dimmisilarity
cl_dissimilarity(k3_ensemb, method = 'Euclidean')

dev.off()

############################################################################
###TECHNIQUE 2:         DAWass: HISTOGRAMS                               ###
############################################################################
library(HistDAWass)

#STEP 1): Preparation of the Datsets for DAWass algorithms
df.ord <- EUrendvol.red[,c('volatility','average_return')] #Ordinary values dataframe

#Function for making a Histogram-list and a Histogram-matrix object (Hist.lst and Hist.mtx)
funcDistributionH <- function(n1){
    cryptsym <- CRYPTO19.DT$FROMSYMBOL[n1]
    CrypHist <- data2hist(CRYP_dep.RT[SYM==cryptsym]$rend, algo = 'base')
    
    return(CrypHist)
}

n1 <- nrow(CRYPTO19.DT)
Hist.lst <- lapply(1:n1, funcDistributionH)
Hist.mtx <- MatH(Hist.lst, nrows = n1)

#STEP 2): Clustering Quality
num_clust <- seq(2,16) #We test
set.seed(123)
Hist_k_results <- lapply(num_clust, function(k) WH_kmeans(Hist.mtx, k, rep = 20))

temp <- c(0)
for (i in 1:length(num_clust)){
    print(Hist_k_results[[i]][['quality']])
    temp<- append(temp,Hist_k_results[[i]][['quality']])
}

plot(seq(1,16),temp,
     main = 'WH-Kmeans: Internal cluster quality',
     xlab = 'Number of clusters',
     ylab = 'Sum of square deviation'
)


#STEP 3) Representation of the Hist DAWass clustering
crypt.his <- rbind.data.frame(c('007','ACP','ALT','ARC','BFX', 'XMO','NBT'),
                              c('BCD','OCN','BITUSD','CHAT','KEY','COIN','DROP'),
                              #c('BTC','ETH','EOS','BCH','XRP','LTC','ICX','HSR','ETC','IOT'),
                              c('BTC','ETH','EOS','BCH','XRP','FRST'),
                              c('NAS','POLY','NKC','JNT','MNTP'),
                              c('B2X','STAR','LBTC','PFR','ITT','XIN*','AMIS','RIPT'),
                              stringsAsFactors = FALSE)
rownames(crypt.his) <- c('Clust1','Clust2','Clust3','Clust4', 'Clust5')
colnames(crypt.his) <- 1:ncol(crypt.his)

df.red <- subset(df.ord, rownames(df.ord) %in% as.vector(t(crypt.his)))

#Figure 19: Histogram DAWass clustering for ordinary variables
p<- fviz_cluster(list(data=df.ord, cluster = Hist_k_results[[4]]$solution$IDX),geom = 'point', labelsize = 7, 
                 show.clust.cent = TRUE,ellipse.alpha = 0,stand = FALSE,
                 main = 'Hist DAWass: Volatility-Mean return')
p <- fviz_add(p,df.red, labelsize = 3, pointsize = 1, color='dimgray',repel = TRUE)
#fviz_add(p, df.cap, labelsize = 3, color = 'red',repel = TRUE)
p

#Cardinality for 5 clusters
sink('Tables/HistKmeansCard.txt')
#Hist_k_results[[6]]$solution$cardinality
Hist_k_results[[4]]$solution$cardinality
sink()

############### Crypto.Qualy DF ##########################################
Crypto.Qualy$HistDAWass <- as.integer(Hist_k_results[[4]]$solution$IDX)
Crypto.Famd$HistDAWass <- as.integer(Hist_k_results[[4]]$solution$IDX)

# ##### IMPORTANT ###################################################################
# ## This update on the script is coded to map script results to the paper
# #The labeling corespondence between script results and the paper are the following
# #Current      Label rename
# #CL1 (16) --> CL5
# #CL2 (57) --> CL4
# #CL3 (496)--> CL2
# #CL4 (147)--> CL1
# #CL5 (1007)-> CL3
# ####################################################################################
# 
# Crypto.Qualy$HistDAWass <- as.factor(Crypto.Qualy$HistDAWass)
# table(Crypto.Qualy$HistDAWass)
# Crypto.Qualy$HistDAWass <- factor(Crypto.Qualy$HistDAWass, labels = c('5','4','2','1','3'))
# table(Crypto.Qualy$HistDAWass)
# Crypto.Qualy$HistDAWass <- as.character(Crypto.Qualy$HistDAWass)
# Crypto.Qualy$HistDAWass <- as.integer(Crypto.Qualy$HistDAWass)
# table(Crypto.Qualy$HistDAWass)
# 
# Crypto.Famd$HistDAWass <- as.factor(Crypto.Famd$HistDAWass)
# table(Crypto.Famd$HistDAWass)
# Crypto.Famd$HistDAWass <- factor(Crypto.Famd$HistDAWass, labels = c('5','4','2','1','3'))
# table(Crypto.Famd$HistDAWass)
# Crypto.Famd$HistDAWass <- as.character(Crypto.Famd$HistDAWass)
# Crypto.Famd$HistDAWass <- as.integer(Crypto.Famd$HistDAWass)
# table(Crypto.Famd$HistDAWass)

#Est. Descriptives of the centroid (--> for the correspondece with the paper firstly apply a
# rename on the labeling)
miCentHist <- Hist_k_results[[4]]$solution$centers
sink('Tables/HistWHCentroids.txt')
miCentHist
sink()

aver <- get.MatH.stats(miCentHist, stat='mean')
desv <- get.MatH.stats(miCentHist, stat='std')
skew <- get.MatH.stats(miCentHist, stat='skewness')
kurt <- get.MatH.stats(miCentHist, stat='kurtosis')
medi <- get.MatH.stats(miCentHist, stat='median')
mini <- get.MatH.stats(miCentHist, stat='min')
maxi <- get.MatH.stats(miCentHist, stat='max')

cv <- desv$mat/aver$mat #Variation coefficient
plot(cv, type = 'line', main = 'Coeficiente de variacion WH-Kmeans', xlab = 'Cluster', ylab = 'Coef. de var')
dev.off()
#Table 2 and Table 3: Descriptive statistics of the cluster prototypes (medoids)
t<-data.frame()
t <- rbind(t, aver$mat) 
t <- cbind(t, desv$mat)
t <- cbind(t, cv)
t <- cbind(t, skew$mat)
t <- cbind(t, kurt$mat)
t <- cbind(t, medi$mat)
t <- cbind(t, mini$mat)
t <- cbind(t, maxi$mat)

# Centroids variance
clustHist <- Hist_k_results[[4]]$solution$IDX
df.var<- data.frame()
for (i in 1:5){
    temp <- (clustHist == i)
    temp2.lst <- MatH(Hist.mtx@M[temp], nrows = sum(temp))
    df.var <- rbind(df.var, WH.var.covar(temp2.lst)[1,1])
}

t <- cbind(t,df.var)

rownames(t)<- c('Clust1','Clust2','Clust3','Clust4','Clust5')
colnames(t)<- c('Mean','Std','Coef.Var.','Skewness','Kurtosis', 
                'Median','min','Max','VarCentr')
t <- round(t, digits = 2)
t

write.table(t, file = "Tables/HistDAWassCaractSts.txt", sep = "\t",
            row.names = TRUE, col.names = NA)

#Figure 5: Cluster's centroids, closer cryptocurrency to the centroids and 
#some of the more representative cryptocurrencies of each cluster in terms 
#of market cap
plot(Hist_k_results[[4]]$solution$centers[1], type="DENS")
plot(Hist_k_results[[4]]$solution$centers[2], type="DENS")
plot(Hist_k_results[[4]]$solution$centers[3], type="DENS")
plot(Hist_k_results[[4]]$solution$centers[4], type="DENS")
plot(Hist_k_results[[4]]$solution$centers[5], type="DENS")

#Benchamring all density plots
plot(Hist_k_results[[4]]$solution$centers[1:5], type="DENS")

#Representing the CDF
plot(Hist_k_results[[4]]$solution$centers[1], type="CDF")
plot(Hist_k_results[[4]]$solution$centers[2], type="CDF")
plot(Hist_k_results[[4]]$solution$centers[3], type="CDF")
plot(Hist_k_results[[4]]$solution$centers[4], type="CDF")
plot(Hist_k_results[[4]]$solution$centers[5], type="CDF")

#Representing some cryptocoins
# vectHist <- c('HEAT', '365', 'ALT', 'ZET', 'CHAT', 'KEY', 
#               'BTC', 'ETH','EOS','ARB','POLY','JNT',
#               'FLLW','B2X','ITT')
vectHist <- c('365', 'ALT', 'CHAT', 'KEY', 
              'BTC', 'ETH','POLY','JNT',
              'B2X','ITT')
Crypto.Qualy[Crypto.Qualy$SYM %in% vectHist,c('SYM','HistDAWass')]

pos2 <- which(CRYPTO19.DT$FROMSYMBOL %in% vectHist)
CRYPTO19.DT$FROMSYMBOL[pos2]
plot(Hist.mtx@M[[pos2[1]]], type="DENS", col="red", border="blue")
plot(Hist.mtx@M[[pos2[2]]], type="DENS", col="red", border="blue")
plot(Hist.mtx@M[[pos2[3]]], type="DENS", col="red", border="blue")
plot(Hist.mtx@M[[pos2[4]]], type="DENS", col="red", border="blue")
plot(Hist.mtx@M[[pos2[5]]], type="DENS", col="red", border="blue")
plot(Hist.mtx@M[[pos2[6]]], type="DENS", col="red", border="blue")
plot(Hist.mtx@M[[pos2[7]]], type="DENS", col="red", border="blue")
plot(Hist.mtx@M[[pos2[8]]], type="DENS", col="red", border="blue")
plot(Hist.mtx@M[[pos2[9]]], type="DENS", col="red", border="blue")
plot(Hist.mtx@M[[pos2[10]]], type="DENS", col="red", border="blue")
# plot(Hist.mtx@M[[pos2[11]]], type="DENS", col="red", border="blue")
# plot(Hist.mtx@M[[pos2[12]]], type="DENS", col="red", border="blue")
# plot(Hist.mtx@M[[pos2[13]]], type="DENS", col="red", border="blue")
# plot(Hist.mtx@M[[pos2[14]]], type="DENS", col="red", border="blue")
# plot(Hist.mtx@M[[pos2[15]]], type="DENS", col="red", border="blue")

plot(Hist_k_results[[4]]$solution$centers[1:5], type="BOXPLOT")
dev.off()

############################################################################
###           TECHNIQUE 3:   TADPole clustering:                         ###
############################################################################
library(dtwclust)

#STEP 1) Definition of the dataframe for the TADPole clustering
# The log-return time serie dataframe is ordinary (not scaled)
df2 <- CRYP.timeWise.rend
df.ord <- EUrendvol.red[,c('volatility','average_return')] #Ordinary values dataframe

#STEP 2) Selection of the Optimum number of TADPole clusters ###############
# cut-off distance (dc) = 2
# windows size =3
# Number of cluster tested from 2 to 15
set.seed(123)
TADPole_k <- tsclust(df2, k=2L:15L, type = 't',
                 control = tadpole_control(dc=2L,
                                           window.size = 3L),
                 seed = 123)

names(TADPole_k) <- paste0("k_", 2L:15L)

# STEP 3: CLUSTERING EVALUATION
TADPole_cvi <- sapply(TADPole_k, cvi, type = 'internal')

sink('Tables/TADPoleCVI.txt')
TADPole_cvi
sink()

# STEP 4: selection of the number of cluster K=3
TADPole_r <- TADPole_k$k_3

# STEP 5: we add TADpole cluster to the Qualy DF
Crypto.Qualy$TADPole <- TADPole_r@cluster
Crypto.Famd$TADPole <-  TADPole_r@cluster

#Cardinality
cardClust <- TADPole_r@clusinfo$size
sink('Tables/TADPoleCard.txt')
cardClust
sink()

#Homogenity
TADPole_r@distance
TADPole_r@clusinfo

temp <- data.frame()
temp <- Crypto.Qualy[,.(SYM, TADPole)]
temp$cldist <- TADPole_r@cldist
temp <- temp[,.(avgdist=mean(cldist), sddist=sd(cldist)),by=TADPole]
temp$cv <- temp$sddist / temp$avgdist 

xtable(temp)

nbClust <- length(TADPole_r@clusinfo$size)

#Centroides
names(TADPole_r@centroids)[1:3]

for(i in 1:nbClust){
        crypto.cluster <- unlist(as.vector(Crypto.Qualy[TADPole==i,'SYM']))
        centroid <- names(TADPole_r@centroids)[i]
        plot(CRYP.timeWise.rend[centroid,],type='l', col='gray',
             ylim=c(-0.2,0.2), 
             xlab='2018-Calendar day', ylab='Daily return')
        legend("topleft", legend = c(paste('Clust',i)), col = 'blue')
        lines(CRYP.timeWise.rend[centroid,],type='l', col='red')
}


#TADPole representations
#Figure 6): Cryptocurrencies daily returns along 2018 
#grouped by clusters for TADPole, red color-line for medoid cluster1, 
#blue color-lines for cluster 2 and green for cluster 3
centroid1 <- names(TADPole_r@centroids)[1]
plot(CRYP.timeWise.rend[centroid1,],type='l', col='red',
     ylim=c(-0.1,0.1), 
     xlab='2018-Calendar day', ylab='Daily return')

centroid2 <- names(TADPole_r@centroids)[2]
lines(CRYP.timeWise.rend[centroid2,],type='l', col='green')

centroid3 <- names(TADPole_r@centroids)[3]
lines(CRYP.timeWise.rend[centroid3,],type='l', col='blue')
legend('bottomright', c('Medoid 1: ZNE','Medoid 2: LINK','Medoid 3:XTO'), text.col = c('red','green','blue'))


# Criptomonedas significativas por cluster
# Cryptocoins closer to the centroids
distTADPole.df <- data.frame()
#distTADPole.vect <- TADPole_r@cldist
distTADPole.df <- rbind(distTADPole.df,TADPole_r@cluster)
distTADPole.df <- cbind(t(distTADPole.df),TADPole_r@cldist)
distTADPole.df <- cbind(distTADPole.df,Crypto.Qualy$SYM)
colnames(distTADPole.df)<- c('Cluster','Dist','SYM')
distTADPole.dt <- data.table(distTADPole.df)

distTADPole.dt <- distTADPole.dt[order(distTADPole.dt$Cluster,distTADPole.dt$Dist),]

VectCryp1 <- c("LTCU","PPC","BSTAR")
VectCryp2 <- c("YAC","ZBC","XPRO","BTC","HSR", "ICX","LTC","XRP")
VectCryp3 <- c("XST","YAY","ZET2","BCH","EOS","ETC","ETH")

#Representation of cluste centroids and closer time-series
#Centroid 1
plot(CRYP.timeWise.rend[centroid1,],type='l', col='red',
     ylim=c(-0.2,0.2), 
     xlab='2018-Calendar day', ylab='Daily return')
lines(CRYP.timeWise.rend[VectCryp1[1],],type='l', col='grey40')
#lines(CRYP.timeWise.rend[VectCryp1[2],],type='l', col='grey45')
#lines(CRYP.timeWise.rend[VectCryp1[3],],type='l', col='grey50')
legend("bottomright", c("LINK","LTCU"),text.col=c('red','grey20'))

#Centroid 2
plot(CRYP.timeWise.rend[centroid2,],type='l', col='blue',
     ylim=c(-0.2,0.2), 
     xlab='2018-Calendar day', ylab='Daily return')
lines(CRYP.timeWise.rend[VectCryp2[1],],type='l', col='grey40')
legend("bottomright", c("XTO","YAC"),text.col=c('blue','grey20'))

#Centroid 3
plot(CRYP.timeWise.rend[centroid3,],type='l', col='green',
     ylim=c(-0.2,0.2), 
     xlab='2018-Calendar day', ylab='Daily return')
lines(CRYP.timeWise.rend[VectCryp3[1],],type='l', col='grey40')
legend("bottomright", c("ZNE","XST"),text.col=c('green','grey20'))

crypt.tad <- rbind.data.frame(c('LINK','LTCU','PPC','SWT','AIR', 'NGC','PLR','ZSC'),
                              c('BTC','HSR','ICX','XTO','YAC',   'XMO','AMIS','RIPT'),
                              c('EOS','ETC','ETH','ZNE','XST',   'XIN*','NBT','LBTC'),
                              stringsAsFactors = FALSE)
rownames(crypt.tad) <- c('Clust1','Clust2','Clust3')
colnames(crypt.tad) <- 1:ncol(crypt.tad)

#Figure 6 (a): Density Plots
library(ggridges)
tadp.df <- data.frame()
for (i in 1:3) {
    temp <- TADPole_r@centroids[[i]]
    tadp.df<-rbind(tadp.df,temp)
    
}
tadp.df <- t(tadp.df)
tadp.df <- as.data.frame(tadp.df)
colnames(tadp.df)<- c('Medoid1','Medoid2','Medoid3')
rownames(tadp.df)<- c()

tadp.df$time <- CRYP.RT[SYM=='BTC']$time

summary(tadp.df)
sd(tadp.df$Medoid3)

#Los pasamos a formato long para comparar las curvas de densidad
tadp.long <- melt(tadp.df, id.vars = c('time'))
colnames(tadp.long)<-c('time','Cluster','Daily_Returns')

ggplot(tadp.long, aes(x =Daily_Returns, y = Cluster)) +
    geom_density_ridges(scale=1) +
    ggtitle("TADPole: clusters density") +
    theme(plot.title = element_text(hjust = 0.5)) +
    xlim(-0.25,0.25)

ggplot(tadp.long, aes(x=Daily_Returns, color=Cluster)) +
    geom_density() +
    xlim(-0.25,0.25)


#Time evolution for the Medoids
p5 <- ggplot(tadp.long, aes(x = time, y = Daily_Returns))
p5 + geom_line(aes(color = Cluster))

p5 <- p5 + geom_line(aes(color = Cluster)) +
    facet_wrap(~Cluster, ncol = 1)

#Fin prueba

#Figure 19: TADPole clustering for ordinary variables
df.red <- subset(df.ord, rownames(df.ord) %in% as.vector(t(crypt.tad)))
p <- fviz_cluster(list(data=df.ord, cluster = TADPole_r@cluster),geom = 'point', 
                  labelsize = 7, show.clust.cent = TRUE, ellipse.alpha = 0,
                  main='TADPole: Volatility-Mean return',
                  stand = FALSE)
p <- fviz_add(p,df.red, labelsize = 3, pointsize = 1, color='dimgray',repel = TRUE)
p


#Representacionn en el plano Rendimiento-Ratio de Sharp
# dfSharpVaR <- scale(EUrisk[,c('SharpR','VaR01')]) #Escalamos los datos
# fviz_cluster(list(data=dfSharpVaR, cluster = TADPole_3$k_3@cluster))

#Density graphics for the TADPole medoids
library(ggridges)

tadp.df <- data.frame()
for (i in 1:3) {
        temp <- TADPole_r@centroids[[i]]
        tadp.df<-rbind(tadp.df,temp)

}
tadp.df <- t(tadp.df)
tadp.df <- as.data.frame(tadp.df)
colnames(tadp.df)<- c('Medoid1','Medoid2','Medoid3')
rownames(tadp.df)<- c()

summary(tadp.df) #Descriptive statistic for TADPole medoids
sd(tadp.df$Medoid1)
sd(tadp.df$Medoid2)
sd(tadp.df$Medoid3)
#Format change wide to long for ggplot
tadp.long <- melt(tadp.df)
colnames(tadp.long)<-c('Cluster','Daily_Returns')

ggplot(tadp.long, aes(x =Daily_Returns, y = Cluster)) +
    geom_density_ridges(scale=1) +
    ggtitle("TADPole: clusters density") +
    theme(plot.title = element_text(hjust = 0.5)) +
    xlim(-0.25,0.25)

#Figure 7: Density plot of the three TADPole medois
ggplot(tadp.long, aes(x=Daily_Returns, color=Cluster)) +
    geom_density() +
    xlim(-0.25,0.25)


#TdistributionH ##################
#Monthly distribution of log time-series for the TADPole clusters
tadp.df$month <- c(0)
nr <- nrow(tadp.df)
#split(tadp.df, rep(1:ceiling(nr/12), each=12, length.out=nr))
tadp.lst <- split(tadp.df, rep(1:12, length.out = nr, each = ceiling(nr/12)))
tadp.months <- data.frame()
for (m in 1:12){
    tadp.lst[[m]]$month <- m
    tadp.months <- rbind(tadp.months, tadp.lst[[m]])
}

tadp.months$month <- as.factor(tadp.months$month)
p <- ggplot(tadp.months, aes(x=month, y=Medoid1)) +
    geom_violin()
p

tadp.months$month <- as.factor(tadp.months$month)
p <- ggplot(tadp.months, aes(x=month, y=Medoid2)) +
    geom_violin()
p

tadp.months$month <- as.factor(tadp.months$month)
p <- ggplot(tadp.months, aes(x=month, y=Medoid3)) +
    geom_violin()
p

#Quaterly distribution for TADPole clusters
tadp.df$quarter <- c(0)
tadp.lst <- split(tadp.df, rep(1:4, length.out = nr, each = ceiling(nr/4)))
tadp.quarter <- data.frame()
for ( q in 1:4){
    tadp.lst[[q]]$quarter <- q
    tadp.quarter <- rbind(tadp.quarter, tadp.lst[[q]])
}
tadp.quarter$quarter <- as.factor(tadp.quarter$quarter)

#Figure 7: Cluster 1:Medoid LINK quaterly distribution
p <- ggplot(tadp.quarter, aes(x=quarter, y=Medoid1)) +
    geom_violin() +
    ylim(-0.2, 0.2)
p + stat_summary(fun.data=mean_sdl, mult=1, 
                 geom="pointrange", color="red")

#Figure 7: Cluster 2: Medoid XTO quaterly distribution
p <- ggplot(tadp.quarter, aes(x=quarter, y=Medoid2)) +
    geom_violin() +
    ylim(-0.2, 0.2)
p + stat_summary(fun.data=mean_sdl, mult=1, 
                 geom="pointrange", color="red")

#Figure 7: Clsuter 3: Medoid ZNE quaterly distribution
p <- ggplot(tadp.quarter, aes(x=quarter, y=Medoid3)) +
    geom_violin() +
    ylim(-0.2, 0.2)
p + stat_summary(fun.data=mean_sdl, mult=1, 
                 geom="pointrange", color="red")


dev.off()

##############################################################################
##### Combi: INTERSECTION OF THE THREE TECHNIQUES ######
##############################################################################
# Tables for each of the techniques
Clust.km <- Crypto.Qualy[,.(SYM, Kmeans)]
Clust.his<- Crypto.Qualy[,.(SYM,HistDAWass)]
Clust.tad<- Crypto.Qualy[,.(SYM,TADPole)]

# Utilizaremos los merge
grupos.km <- unique(Clust.km$Kmeans)
grupos.his <- unique(Clust.his$HistDAWass)
grupos.tad <- unique(Clust.tad$TADPole)


r <- c(0)
temp.merge <- data.frame()
Clust.terna <- data.frame()
for (i in grupos.km){
    for (j in grupos.tad){
        temp.merg <- merge(Clust.km[Kmeans==i],Clust.tad[TADPole==j], by = "SYM")
        for (k in grupos.his){
            Clust.kmtadhis <- merge(temp.merg,Clust.his[HistDAWass==k], by = "SYM")
            if(nrow(Clust.kmtadhis)>0){
                r=r+1;
                Clust.kmtadhis$Combi <- r
                Clust.terna <- rbind(Clust.terna,Clust.kmtadhis)
                
            }
        }
    }
}

Clust.terna <- Clust.terna[,.(SYM, Kmeans, HistDAWass, TADPole,.N),by=Combi]
write.table(Clust.terna, file = "Tables/ClustIntersecc.txt", sep = "\t",
            row.names = TRUE, col.names = NA)

#Cardinality for the interesection of the clusters for the three techniques
h2=hist(Clust.terna$Combi, breaks = 40, 
        main = "Cardinality of the groups common to the three techniques",
        xlab = "Group number",
        ylab = "Number of cryptocurrencies")
text(h2$mids,h2$counts, labels = h2$counts, adj = c(0.5, -0.5), cex = 0.6 )
plot(ecdf(h2$counts), main="Cardinality ACDF",
     xlab = "Cardinality")


# Higher cardinality cluster intersections
Clust.terna.ord <- Clust.terna[order(Clust.terna$N,decreasing = TRUE),]
Clust.terna.ord <- unique(Clust.terna.ord[,.(Kmeans,HistDAWass,TADPole,Combi,N)])
Clust.terna.ord$Combi <- as.integer(Clust.terna.ord$Combi)

#Table 4: Cluster intersection through the different clustering techniques
Clust.terna.ord
write.table(Clust.terna.ord, file = "Tables/ClustIntersecc.txt", sep = "\t",
            row.names = TRUE, col.names = NA)

xtable(Clust.terna.ord)

#We rename Combi labels
Clust.terna$Combi <- as.factor(Clust.terna$Combi)

for (i in 1:nrow(Clust.terna.ord)){
    nfila <- which(Clust.terna.ord$Combi == i)
    levels(Clust.terna$Combi)[i] <- nfila + 100
}

Clust.terna$Combi <- as.character(Clust.terna$Combi)
Clust.terna$Combi <- as.integer(Clust.terna$Combi)
Clust.terna$Combi <- Clust.terna$Combi - 100

unique(Clust.terna$Combi)
#We add Combi to the Qualy table
Crypto.Qualy <- merge(Crypto.Qualy, Clust.terna[,c(-3,-4,-5,-6)], all.x = TRUE)

#We rename Combi labels ##########################################


# Crypto.Qualy$Combi <- factor(Crypto.Qualy$Combi,
#                              labels = as.character(Clust.terna.ord$Combi))
# Crypto.Qualy$Combi <- as.character(Crypto.Qualy$Combi) #Pasamos Combi de Factor a String
# Crypto.Qualy$Combi <- as.integer(Crypto.Qualy$Combi) #Pasamos Combi de Factor a String

# Crypto.Qualy[Crypto.Qualy$SYM=='BTC','Combi']
# Crypto.Qualy[Crypto.Qualy$SYM=='ALT','Combi']


######### Re-naming of the Qualy table  #####
colnames(Crypto.Qualy) <- c("SYM","Algorithm","ProofType","PercVolume","ClassVolume","PercMKCaP","ClassMKCaP",
                            "ClassVolumeUSD","ClassRet","ClassRisk","DecVolumeUSD","DecRet","DecRisk",
                            "ClassSharpeR","ClassBeta","DecAge","Kmeans","HistDAWass","TADPole","Combi" )

# colnames(Crypto.Qualy) <- c("SYM","Algorithm","ProofType","PercMKCap","ClassMKCap","ClassVolu","ClassRet",
#                                 "ClassRisk","DecVolu","DecRet","DecRisk","ClassSharpR","DecM2","DecM2Sort",
#                                 "DecSortRat","ClassBeta","DecAge","Kmeans","HistDAWass","TADPole","Combi" )



# Reordenamos las variables de la tabla
# setcolorder(Crypto.Qualy, c("SYM","Algorithm","ProofType","PercMKCap","ClassMKCap","DecAge","ClassVolu","ClassRet",
#                                 "ClassRisk","DecVolu","DecRet","DecRisk","ClassSharpR","DecM2","DecM2Sort",
#                                 "DecSortRat","ClassBeta","Kmeans","HistDAWass","TADPole","Combi"))
#Crypto.Qualy$Combi <- as.integer(Crypto.Qualy$Combi)
write.table(Crypto.Qualy, file = "Tables/CryptoQualy.txt", sep = "\t",
            row.names = TRUE, col.names = NA)
Crypto.Famd$Combi <- Crypto.Qualy$Combi
write.table(Crypto.Famd, file = "Tables/CryptoFamd.txt", sep = "\t",
            row.names = TRUE, col.names = NA)

########################################################################################
## Descriptive Statistics for the clusters of each technique ###########################
########################################################################################
## Centroid coordinates K-means
df.ord <- EUrendvol.red[,c('volatility','average_return')] #Ordinary values dataframe
df <- scale(EUrendvol.red[,c('volatility','average_return')]) #Scalated values dataframe

mu <- apply(df.ord,2,mean)
n <- 3
mu <- do.call("rbind", replicate(n, mu, simplify = FALSE))
mu

sd <- apply(df.ord,2,sd)
sd <- do.call("rbind", replicate(n, sd, simplify = FALSE))
sd

temp <- matrix(k3_medoid$centers[1:6],nrow = 3)
temp
k3_medoid_ord <- temp*sd + mu
colnames(k3_medoid_ord)<- c('volatility','average_return')
rownames(k3_medoid_ord)<- c('Cluster 1','Cluster 2','Cluster 3')
k3_medoid_ord

write.table(k3_medoid_ord, file = "Tables/CartdTableKmeans.txt", sep = "\t",
            row.names = TRUE, col.names = NA)


################################################# Heavy tails #########################################
Crypto.Qualy.Heavy <- Crypto.Qualy[!Crypto.Qualy$SYM %in% PLD.HQ$Ticker,c('SYM','Algorithm','ProofType',
                                                                          'PercVolume',
                                                                          'PercMKCaP',
                                                                          'ClassSharpeR','ClassBeta',
                                                                          'DecAge','Kmeans','HistDAWass',
                                                                          'TADPole','Combi')]

Crypto.Qualy.Heavy$Kmeans <- as.factor(Crypto.Qualy.Heavy$Kmeans)
table(Crypto.Qualy.Heavy$Kmeans)

Crypto.Qualy.Heavy$HistDAWass <- as.factor(Crypto.Qualy.Heavy$HistDAWass)
table(Crypto.Qualy.Heavy$HistDAWass)

Crypto.Qualy.Heavy$TADPole <- as.factor(Crypto.Qualy.Heavy$TADPole)
table(Crypto.Qualy.Heavy$TADPole)


#################################################################################
###                   We save important variables                             ###
#################################################################################

nfich1 <- c("Dataset/ClusterResultsFocusRev1.RData")
#save(k3_iter, Hist_k_results, Hist.mtx, TADPole_r ,TADPole_k, file = nfich1)
#load(nfich1)

nfich2 <- c("Dataset/QualyQuantRev1.RData")
#save(EUrisk, EUrendvol, MKTCAPShare, Crypto.Qualy, Crypto.Famd, file = nfich2)
#load(nfich3)

##################################################################################
###                                       Referencias                          ###
##################################################################################

#Ref.1: Multivariate Analysis I by Alboukadel Kassambara Practical Guide to Cluster
# Analysis in R. Unsepervised Machine Learning

#Ref.2: https://lukedaniels1.github.io/Bio381_2018/Daniels_Cluster_Analysis_Lecture.html
#Ref_3: https://www.statmethods.net/stats/frequencies.html
