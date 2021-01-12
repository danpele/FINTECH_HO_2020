#####################################################################
##### FASE 3: Association tests                                 #####
##### Association test on the lcuster results
#
#####################################################################

#LIBRERIAS
library(reshape2)
library(dplyr)
library(data.table)

#DATASET
setwd('D:/DOCTORADO/Scripts/R/Revision1/')
source('CryptFunctionsV10.R')
#Dataset 2018

nfich1 <- c("Dataset/QualyQuantRev1.RData")
load(nfich1)

nfich2 <- c("Dataset/MyPLDIndex.RData")
load(nfich2)
###########################################################################
###           Chi-Square Test                                           ###
### H0: Both variables are independet
### Ha: Both variables are associated one to each other
###########################################################################


# STEp 1: Processing of the Crypto.Qualy table
#WE make inference only for higher cubic rate distribution time-series
Crypto.Qualy.red <- Crypto.Qualy[Crypto.Qualy$SYM %in% PLD.HQ$Ticker,c('Algorithm','ProofType',
                                                                       'PercVolume',
                                                                       'PercMKCaP',
                                                                       'ClassSharpeR','ClassBeta',
                                                                       'DecAge','Kmeans','HistDAWass',
                                                                       'TADPole','Combi')]
#Focoused on the relevant variables for the association tests
#Crypto.Qualy.red <- Crypto.Qualy.red[,c(-1,-5,-7,-8,-9,-10,-11,-12)] #Quitamos algunas variables

#Renaming some variables
#colnames(Crypto.Qualy.red) <- c("Algorithm","ProofType","MkCap","Age","Sharp","M2","M2Sort",
#                     "Sort","Beta","Kmeans","HistDAWass","TADPole","Combi")

#Re-ordering of the variables
#setcolorder(Crypto.Qualy.red, c("Algorithm","ProofType","MkCap","Beta","M2","M2Sort","Sort",
#                                "Sharp","Age","Kmeans","HistDAWass","TADPole","Combi"))

#We apply a data.frame format required for the association functions
Crypto.Qualy.red <- as.data.frame(Crypto.Qualy.red)

#The table DepVarCat.Fisher is filled with the p-values of all association tests
DepVarCat.Fisher <- matrix(nrow = ncol(Crypto.Qualy.red), ncol = ncol(Crypto.Qualy.red), byrow = FALSE)
colnames(DepVarCat.Fisher) <- colnames(Crypto.Qualy.red)
rownames(DepVarCat.Fisher) <- colnames(Crypto.Qualy.red)


# ### We process the intersection of clusters (rename of subcluster focouse on the 6's higher cardinality)
# Combi10 <- c(6,2,16,12,18,14) #Identificadores de las principaels intersecciones de clusters 
# #We filter the higher cardinality
# Crypto.Qualy.red2 <- subset(Crypto.Qualy.red, Combi %in% Combi10)
# Crypto.Qualy.red2$Combi <- as.factor(Crypto.Qualy.red2$Combi)
# #We rename the intersections
# Crypto.Qualy.red2$Combi <- factor(Crypto.Qualy.red2$Combi,
#                                   labels = c("6","2","16","12","18","14"))
# Crypto.Qualy.red2$Combi <- as.integer(Crypto.Qualy.red2$Combi) #Pasamos Combi de Factor a String


#STEP 2: ASSOCIATION TEST computation
set.seed(123)
for (i in 1:ncol(Crypto.Qualy.red)){        #For columns
        for (j in i:ncol(Crypto.Qualy.red)){#For rows
                if (i==j) {
                        DepVarCat.Fisher[i,j]=0
                        #DepVarCat.Cramer[i,j]=1
                        next}
                tempContingTabl <- table(Crypto.Qualy.red[,i],Crypto.Qualy.red[,j])
                
                #Fisher's exact test
                temp1 <- chisq.test(tempContingTabl, 
                                                 simulate.p.value = TRUE, B=8000)$p.value
                DepVarCat.Fisher[i,j] <- DepVarCat.Fisher[j,i] <- temp1
        }
}

#p-values for all association tests
write.table(DepVarCat.Fisher, file = "Tables/FisherTestPvalues.txt", sep = ",",
            row.names = TRUE, col.names = NA)

#STEP 3: Processing of association results
# We highlight more relevant associations setting a threshold for the p-values (>0.01)
temp <- DepVarCat.Fisher
temp1 <- temp > 0.01
temp[temp1] <- 1 
set.seed(123)
heatmap(1-temp, Colv = NA, Rowv = NA, 
        col = cm.colors(256), symm = TRUE,revC = TRUE,
        cexRow = 0.9, cexCol = 0.9
)

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


#STEP 4: PEARSON'S RESIDUALS
library(vcd) #Library for Person's residula representation

########### PercVolume - Kmeans ########################################
Feat <- Crypto.Qualy.red[,3]
Feat <- as.factor(Feat)
Feat <- factor(Feat, levels=c('NA','P70','P80','P90','P99','P100'))
Feat <- factor(Feat) #Cleaning of feature

ClustFeat <- Crypto.Qualy.red[,8]
ClustFeat <- as.factor(ClustFeat)

PercVolume <- Feat
ClustKmeans <- ClustFeat

#tempContingTabl <- table(Feat,ClustFeat)
tempContingTabl <- table(ClustKmeans,PercVolume)
mosaicplot(tempContingTabl, shade = TRUE, las = 2, 
           main = 'Volume VS Cluster Kmeans')

sink('Tables/ContingTablPercVolume_Kmeans.txt')
tempContingTabl
sink()

set.seed(123)
temp.Chi <- chisq.test(tempContingTabl, 
                       simulate.p.value = TRUE, B=8000)
assoc(tempContingTabl, shade = TRUE)
Assoc <- residPearson.mtx(temp.Chi$residuals, temp.Chi$observed)
Assoc
temp.Chi$observed

sink('Tables/PearsTablVolume_Kmeans.txt')
temp.Chi$stdres
sink()

xtable(temp.Chi$stdres)

######### PercVolume - Hist DAWass ########################################
Feat <- Crypto.Qualy.red[,3]
Feat <- as.factor(Feat)
Feat <- factor(Feat, levels=c('NA','P70','P80','P90','P99','P100'))
Feat <- factor(Feat) #Elimina los factores que no tienen observaciones

ClustFeat <- Crypto.Qualy.red[,9]
ClustFeat <- as.factor(ClustFeat)

PercVolume <- Feat
ClustDAWass <- ClustFeat

#tempContingTabl <- table(Feat,ClustFeat)
tempContingTabl <- table(ClustDAWass,PercVolume)
mosaicplot(tempContingTabl, shade = TRUE, las = 2, 
           main = 'Marketcap VS Cluster Hist DAWass')

sink('Tables/ContingTabPercVolume_DAWass.txt')
tempContingTabl
sink()

set.seed(123)
temp.Chi <- chisq.test(tempContingTabl, 
                       simulate.p.value = TRUE, B=8000)
assoc(tempContingTabl, shade = TRUE)
Assoc <- residPearson.mtx(temp.Chi$residuals, temp.Chi$observed)
Assoc
temp.Chi$observed

sink('Tables/PearsTablVolume_DAWass.txt')
temp.Chi$stdres
sink()

xtable(temp.Chi$stdres)

########### PercMKCaP - Kmeans ########################################
Feat <- Crypto.Qualy.red[,4]
Feat <- as.factor(Feat)
Feat <- factor(Feat, levels=c('NA','P70','P80','P90','P99','P100'))
Feat <- factor(Feat) #Cleaning of feature

ClustFeat <- Crypto.Qualy.red[,8]
ClustFeat <- as.factor(ClustFeat)

PercMKCaP <- Feat
ClustKmeans <- ClustFeat

#tempContingTabl <- table(Feat,ClustFeat)
tempContingTabl <- table(ClustKmeans,PercMKCaP)
mosaicplot(tempContingTabl, shade = TRUE, las = 2, 
           main = 'PercMKCaP VS Cluster Kmeans')

sink('ContingTablPercMKCaP_Kmeans.txt')
tempContingTabl
sink()

set.seed(123)
temp.Chi <- chisq.test(tempContingTabl, 
                       simulate.p.value = TRUE, B=8000)
assoc(tempContingTabl, shade = TRUE)
Assoc <- residPearson.mtx(temp.Chi$residuals, temp.Chi$observed)
Assoc
temp.Chi$observed

sink('Tables/PearsTablMkCap_Kmeans.txt')
temp.Chi$stdres
sink()

xtable(temp.Chi$stdres)

######### PercMKCaP - Hist DAWass ########################################
Feat <- Crypto.Qualy.red[,4]
Feat <- as.factor(Feat)
Feat <- factor(Feat, levels=c('NA','P70','P80','P90','P99','P100'))
Feat <- factor(Feat) #Elimina los factores que no tienen observaciones

ClustFeat <- Crypto.Qualy.red[,9]
ClustFeat <- as.factor(ClustFeat)

PercMKCaP <- Feat
ClustDAWass <- ClustFeat

#tempContingTabl <- table(Feat,ClustFeat)
tempContingTabl <- table(ClustDAWass,PercMKCaP)
mosaicplot(tempContingTabl, shade = TRUE, las = 2, 
           main = 'Marketcap VS Cluster Hist DAWass')

sink('Tables/ContingTabPercMKCaP_DAWass.txt')
tempContingTabl
sink()

set.seed(123)
temp.Chi <- chisq.test(tempContingTabl, 
                       simulate.p.value = TRUE, B=8000)
assoc(tempContingTabl, shade = TRUE)
Assoc <- residPearson.mtx(temp.Chi$residuals, temp.Chi$observed)
Assoc
temp.Chi$observed

sink('Tables/PearsTablMkCap_DAWass.txt')
temp.Chi$stdres
sink()

xtable(temp.Chi$stdres)

# Ratio de Sharpe - TADPole ##############################################
Feat <- Crypto.Qualy.red[,5]
Feat <- as.factor(Feat)
Feat <- factor(Feat, levels=c('NA','SRF','ERP','Acc','GOOD'))
Feat <- factor(Feat) #Elimina los factores que no tienen observaciones

ClustFeat <- Crypto.Qualy.red[,10]
ClustFeat <- as.factor(ClustFeat)

SharpeR <- Feat
ClusTADPole <- ClustFeat

#tempContingTabl <- table(Feat,ClustFeat)
tempContingTabl <- table(ClusTADPole,SharpeR)
mosaicplot(tempContingTabl, shade = TRUE, las = 2, 
           main = 'Sharpe ratio vs TADPole')

sink('Tables/TabFreqSharpeTADPole.txt')
tempContingTabl
sink()

set.seed(123)
temp.Chi <- chisq.test(tempContingTabl, 
                       simulate.p.value = TRUE, B=8000)
assoc(tempContingTabl, shade = TRUE)
Assoc <- residPearson.mtx(temp.Chi$residuals, temp.Chi$observed)
Assoc
temp.Chi$observed

sink('Tables/TabAsocSharpeTADPole.txt')
temp.Chi$stdres
sink()

xtable(temp.Chi$stdres)


######### Pearson's residual for K-means and Beta
Feat <- Crypto.Qualy.red[,6]
Feat <- as.factor(Feat)
Feat <- factor(Feat, levels=c('NegBeta','CashLike','LowVol','Indexlike',
                              'HighVol','Extreme'))

ClustFeat <- Crypto.Qualy.red[,8]
ClustFeat <- as.factor(ClustFeat)

Beta <- Feat
ClustKmeans <- ClustFeat

#tempContingTabl <- table(Feat,ClustFeat)
tempContingTabl <- table(ClustKmeans,Beta)
mosaicplot(tempContingTabl, shade = TRUE, las = 2, 
           main = 'Beta VS K-means cluster')

sink('Tables/TabFreqBetaKMeans.txt')
tempContingTabl
sink()

set.seed(123)
temp.Chi <- chisq.test(tempContingTabl, 
                       simulate.p.value = TRUE, B=8000)
assoc(tempContingTabl, shade = TRUE)
Assoc <- residPearson.mtx(temp.Chi$residuals, temp.Chi$observed)
Assoc

temp.Chi$observed
sink('Tables/TabAsocBetaKMeans.txt')
temp.Chi$stdres
sink()

xtable(temp.Chi$stdres)

######### Pearson's residual for Hist DAWass and Beta
Feat <- Crypto.Qualy.red[,6]
Feat <- as.factor(Feat)
Feat <- factor(Feat, levels=c('NegBeta','CashLike','LowVol','Indexlike',
                              'HighVol','Extreme'))

ClustFeat <- Crypto.Qualy.red[,9]
ClustFeat <- as.factor(ClustFeat)

Beta <- Feat
ClustHistDAwass <- ClustFeat

#tempContingTabl <- table(Feat,ClustFeat)
tempContingTabl <- table(ClustHistDAwass,Beta)
mosaicplot(tempContingTabl, shade = TRUE, las = 2, 
           main = 'Beta VS Hist DAWass cluster')

sink('Tables/TabFreqBetaDAWass.txt')
tempContingTabl
sink()

set.seed(123)
temp.Chi <- chisq.test(tempContingTabl, 
                       simulate.p.value = TRUE, B=8000)
assoc(tempContingTabl, shade = TRUE)
Assoc <- residPearson.mtx(temp.Chi$residuals, temp.Chi$observed)
Assoc

temp.Chi$observed
sink('Tables/TabAsocBetaKMeans.txt')
temp.Chi$stdres
sink()

xtable(temp.Chi$stdres)

## Figure 18.a) Age - Kmeans
# Edad - Kmeans ##############################################
Feat <- Crypto.Qualy.red[,7]
Feat <- as.factor(Feat)
Feat <- factor(Feat, levels=c('NA','D4','D5','D6','D7','D8','D9','D10'))
Feat <- factor(Feat) #Elimina los factores que no tienen observaciones

ClustFeat <- Crypto.Qualy.red[,8]
ClustFeat <- as.factor(ClustFeat)

#tempContingTabl <- table(Feat,ClustFeat)
ClustKmeans <- ClustFeat
Maturity <- Feat
tempContingTabl <- table(ClustKmeans,Maturity)
mosaicplot(tempContingTabl, shade = TRUE, las = 2, 
           main = 'Maturity vs K-means')

sink('Tables/TabFreqAgeKmeans.txt')
tempContingTabl
sink()

set.seed(123)
temp.Chi <- chisq.test(tempContingTabl, 
                       simulate.p.value = TRUE, B=8000)
assoc(tempContingTabl, shade = TRUE)
Assoc <- residPearson.mtx(temp.Chi$stdres, temp.Chi$observed)
Assoc

temp.Chi$observed
sink('Tables/TabAsocAgeKmeans.txt')
temp.Chi$stdres
sink()

xtable(temp.Chi$stdres)

## Age - WH-Kmeans
# Edad - WH-Kmeans ##############################################
Feat <- Crypto.Qualy.red[,7]
Feat <- as.factor(Feat)
Feat <- factor(Feat, levels=c('NA','D4','D5','D6','D7','D8','D9','D10'))
Feat <- factor(Feat) #Elimina los factores que no tienen observaciones

ClustFeat <- Crypto.Qualy.red[,9]
ClustFeat <- as.factor(ClustFeat)

#tempContingTabl <- table(Feat,ClustFeat)
Maturity <- Feat
ClustDAWass <- ClustFeat
tempContingTabl <- table(ClustDAWass,Maturity)
mosaicplot(tempContingTabl, shade = TRUE, las = 2, 
           main = 'Maturity vs Histo-DAWass')

sink('Tables/TabFreqAgeDAWass.txt')
tempContingTabl
sink()

set.seed(123)
temp.Chi <- chisq.test(tempContingTabl, 
                       simulate.p.value = TRUE, B=8000)
assoc(tempContingTabl, shade = TRUE)
Assoc <- residPearson.mtx(temp.Chi$stdres, temp.Chi$observed)
Assoc

temp.Chi$observed
sink('Tables/TabAsocAgeDAWass.txt')
temp.Chi$stdres
sink()

xtable(temp.Chi$stdres)

### Technological variables ########################################
# ProofType - PercVolume ##############################################
Feat <- Crypto.Qualy.red[,3]
Feat <- as.factor(Feat)
Feat <- factor(Feat, levels=c('NA','P70','P80','P90','P99','P100'))
Feat <- factor(Feat) #Elimina los factores que no tienen observaciones

ClustFeat <- Crypto.Qualy.red[,2]
ClustFeat <- as.factor(ClustFeat)
#ClustFeat <- factor(ClustFeat)

#tempContingTabl <- table(Feat,ClustFeat)
PercVolume <- Feat
ProofType <- ClustFeat
tempContingTabl <- table(ProofType,PercVolume)
mosaicplot(tempContingTabl, shade = TRUE, las = 2, 
           main = 'ProofType vs PercVolume')

sink('Tables/TabFreqProofVolume.txt')
tempContingTabl
sink()

set.seed(123)
temp.Chi <- chisq.test(tempContingTabl, 
                       simulate.p.value = TRUE, B=8000)
assoc(tempContingTabl, shade = TRUE)
Assoc <- residPearson.mtx(temp.Chi$stdres, temp.Chi$observed)
Assoc

temp.Chi$observed
sink('Tables/TabAsocProofVolume.txt')
temp.Chi$stdres
sink()

xtable(temp.Chi$stdres)

# ProofType - PercMKCaP ##############################################
Feat <- Crypto.Qualy.red[,4]
Feat <- as.factor(Feat)
Feat <- factor(Feat, levels=c('NA','P70','P80','P90','P99','P100'))
Feat <- factor(Feat) #Elimina los factores que no tienen observaciones

ClustFeat <- Crypto.Qualy.red[,2]
ClustFeat <- as.factor(ClustFeat)
#ClustFeat <- factor(ClustFeat)

#tempContingTabl <- table(Feat,ClustFeat)
PercMKCaP <- Feat
ProofType <- ClustFeat
tempContingTabl <- table(ProofType,PercMKCaP)
mosaicplot(tempContingTabl, shade = TRUE, las = 2, 
           main = 'ProofType vs PercMKCaP')

sink('Tables/TabFreqProofMKCaP.txt')
tempContingTabl
sink()

set.seed(123)
temp.Chi <- chisq.test(tempContingTabl, 
                       simulate.p.value = TRUE, B=8000)
assoc(tempContingTabl, shade = TRUE)
Assoc <- residPearson.mtx(temp.Chi$stdres, temp.Chi$observed)
Assoc

temp.Chi$observed
sink('Tables/TabAsocProofMkCap.txt')
temp.Chi$stdres
sink()

xtable(temp.Chi$stdres)


# Algorithm - PercMKCaP ##############################################
Feat <- Crypto.Qualy.red[,4]
Feat <- as.factor(Feat)
Feat <- factor(Feat, levels=c('NA','P70','P80','P90','P99','P100'))
Feat <- factor(Feat) #Elimina los factores que no tienen observaciones

ClustFeat <- Crypto.Qualy.red[,1]
ClustFeat <- as.factor(ClustFeat)
#ClustFeat <- factor(ClustFeat)

#tempContingTabl <- table(Feat,ClustFeat)
PercMKCaP <- Feat
Algorithm <- ClustFeat
tempContingTabl <- table(Algorithm,PercMKCaP)
mosaicplot(tempContingTabl, shade = TRUE, las = 2, 
           main = 'Algorithm vs PercMKCaP')

sink('Tables/TabFreqAlgorithmMKCaP.txt')
tempContingTabl
sink()

set.seed(123)
temp.Chi <- chisq.test(tempContingTabl, 
                       simulate.p.value = TRUE, B=8000)
assoc(tempContingTabl, shade = TRUE)
Assoc <- residPearson.mtx(temp.Chi$stdres, temp.Chi$observed)
Assoc

temp.Chi$observed
sink('Tables/TabAsocAlgorithmMkCap.txt')
temp.Chi$stdres
sink()

xtable(temp.Chi$stdres)

###################################################################
#                     Association for intersections               #
###################################################################
Crypto.Qualy.Inter <-  Crypto.Qualy.red[Crypto.Qualy.red$Combi < 7,]

## Volume ##############################
Feat <- Crypto.Qualy.Inter[,3]
Feat <- as.factor(Feat)
Feat <- factor(Feat, levels=c('NA','P70','P80','P90','P99','P100'))
Feat <- factor(Feat) #Elimina los factores que no tienen observaciones

ClustFeat <- Crypto.Qualy.Inter[,11]
ClustFeat <- as.factor(ClustFeat)
#ClustFeat <- factor(ClustFeat)

#tempContingTabl <- table(Feat,ClustFeat)
PercVolume <- Feat
ClustCombi <- ClustFeat

#tempContingTabl <- table(Feat,ClustFeat)
tempContingTabl <- table(ClustCombi,PercVolume)
mosaicplot(tempContingTabl, shade = TRUE, las = 2, 
           main = 'PercVolume VS Combi cluster')

sink('Tables/TabFreqPercVolumeCombi.txt')
tempContingTabl
sink()

set.seed(123)
temp.Chi <- chisq.test(tempContingTabl, 
                       simulate.p.value = TRUE, B=8000)
assoc(tempContingTabl, shade = TRUE)
Assoc <- residPearson.mtx(temp.Chi$residuals, temp.Chi$observed)
Assoc

temp.Chi$observed
sink('Tables/TabAsocPercVolumeCombi.txt')
temp.Chi$stdres
sink()

xtable(temp.Chi$stdres)

## Market Cap ##############################
Feat <- Crypto.Qualy.Inter[,4]
Feat <- as.factor(Feat)
Feat <- factor(Feat, levels=c('NA','P70','P80','P90','P99','P100'))
Feat <- factor(Feat) #Elimina los factores que no tienen observaciones

ClustFeat <- Crypto.Qualy.Inter[,11]
ClustFeat <- as.factor(ClustFeat)
#ClustFeat <- factor(ClustFeat)

#tempContingTabl <- table(Feat,ClustFeat)
PercMKCaP <- Feat
ClustCombi <- ClustFeat

#tempContingTabl <- table(Feat,ClustFeat)
tempContingTabl <- table(ClustCombi,PercMKCaP)
mosaicplot(tempContingTabl, shade = TRUE, las = 2, 
           main = 'PercMKCap VS Combi cluster')

sink('Tables/TabFreqPercMKCapCombi.txt')
tempContingTabl
sink()

set.seed(123)
temp.Chi <- chisq.test(tempContingTabl, 
                       simulate.p.value = TRUE, B=8000)
assoc(tempContingTabl, shade = TRUE)
Assoc <- residPearson.mtx(temp.Chi$residuals, temp.Chi$observed)
Assoc

temp.Chi$observed
sink('Tables/TabAsocPercMKCapCombi.txt')
temp.Chi$stdres
sink()

xtable(temp.Chi$stdres)

## Sharpe ratio ##############################################
Feat <- Crypto.Qualy.Inter[,5]
Feat <- as.factor(Feat)
Feat <- factor(Feat, levels=c('NA','SRF','ERP','Acc','GOOD'))
Feat <- factor(Feat) #Elimina los factores que no tienen observaciones

ClustFeat <- Crypto.Qualy.Inter[,11]
ClustFeat <- as.factor(ClustFeat)

SharpeR <- Feat
ClustCombi <- ClustFeat

#tempContingTabl <- table(Feat,ClustFeat)
tempContingTabl <- table(ClustCombi,SharpeR)
mosaicplot(tempContingTabl, shade = TRUE, las = 2, 
           main = 'Sharpe ratio vs Combi')

sink('Tables/TabFreqSharpeCombi.txt')
tempContingTabl
sink()

set.seed(123)
temp.Chi <- chisq.test(tempContingTabl, 
                       simulate.p.value = TRUE, B=8000)
assoc(tempContingTabl, shade = TRUE)
Assoc <- residPearson.mtx(temp.Chi$residuals, temp.Chi$observed)
Assoc
temp.Chi$observed

sink('Tables/TabAsocSharpeCombi.txt')
temp.Chi$stdres
sink()

xtable(temp.Chi$stdres)


######### Beta ############################################
Feat <- Crypto.Qualy.Inter[,6]
Feat <- as.factor(Feat)
Feat <- factor(Feat, levels=c('NegBeta','CashLike','LowVol','Indexlike',
                              'HighVol')) #We remove "Extreme" level-factor

Feat <- as.factor(Feat)
ClustFeat <- Crypto.Qualy.Inter[,11]
ClustFeat <- as.factor(ClustFeat)

Beta <- Feat
ClustCombi <- ClustFeat

#tempContingTabl <- table(Feat,ClustFeat)
tempContingTabl <- table(ClustCombi,Beta)
mosaicplot(tempContingTabl, shade = TRUE, las = 2, 
           main = 'Beta VS Combi cluster')

sink('Tables/TabFreqBetaCombi.txt')
tempContingTabl
sink()

set.seed(123)
temp.Chi <- chisq.test(tempContingTabl, 
                       simulate.p.value = TRUE, B=8000)
assoc(tempContingTabl, shade = TRUE)
Assoc <- residPearson.mtx(temp.Chi$residuals, temp.Chi$observed)
Assoc

temp.Chi$observed
sink('Tables/TabAsocBetaCombi.txt')
temp.Chi$stdres
sink()

xtable(temp.Chi$stdres)

######### Maturity ############################################
Feat <- Crypto.Qualy.Inter[,7]
Feat <- as.factor(Feat)
Feat <- factor(Feat, levels=c('D4','D5','D6','D7','D8','D9','D10'))
Feat <- as.factor(Feat)

ClustFeat <- Crypto.Qualy.Inter[,11]
ClustFeat <- as.factor(ClustFeat)

Age <- Feat
ClustCombi <- ClustFeat

#tempContingTabl <- table(Feat,ClustFeat)
tempContingTabl <- table(ClustCombi,Age)
mosaicplot(tempContingTabl, shade = TRUE, las = 2, 
           main = 'Age VS Combi cluster')

sink('Tables/TabFreqAgeCombi.txt')
tempContingTabl
sink()

set.seed(123)
temp.Chi <- chisq.test(tempContingTabl, 
                       simulate.p.value = TRUE, B=8000)
assoc(tempContingTabl, shade = TRUE)
Assoc <- residPearson.mtx(temp.Chi$residuals, temp.Chi$observed)
Assoc

temp.Chi$observed
sink('Tables/TabAsocAgeCombi.txt')
temp.Chi$stdres
sink()

xtable(temp.Chi$stdres)

################### SCREENING ######################################
### We consider better cryptocoins from statistical point of view (PLD.HQ)
Crypto.Qualy.Cubic <- Crypto.Qualy[Crypto.Qualy$SYM %in% PLD.HQ$Ticker,c('SYM','Algorithm','ProofType',
                                                                         'PercVolume',
                                                                         'PercMKCaP',
                                                                         'ClassSharpeR','ClassBeta',
                                                                         'DecAge','Kmeans','HistDAWass',
                                                                         'TADPole','Combi')]


### Clusters Kmeans ##############
Crypto.Qualy.Cubic[Crypto.Qualy.Cubic$PercMKCaP=='P100' & Crypto.Qualy.Cubic$PercVolume=='P100',]
#Cluster 3
Crypto.Qualy.Cubic[Kmeans==3 & ClassBeta=='NegBeta' & PercMKCaP=='P99',.(SYM, PercMKCaP, ClassBeta, Kmeans)]
Crypto.Qualy.Cubic[Kmeans==3 & ClassBeta=='HighVol' & PercMKCaP=='P99',.(SYM, PercMKCaP, ClassBeta, Kmeans)]

#Cluster 1
Crypto.Qualy.Cubic[Kmeans==1,.(SYM, PercMKCaP, ClassBeta, Kmeans)]



#Cluster 2
Crypto.Qualy.Cubic[Kmeans==2,.(SYM, PercMKCaP, Kmeans)]
Crypto.Qualy.Cubic[Kmeans==2 & ClassBeta=='CashLike' & PercMKCaP=='P70',.(SYM, PercMKCaP, ClassBeta, Kmeans)]
Crypto.Qualy.Cubic[Kmeans==2 & ClassBeta=='LowVol' & PercMKCaP=='P100',.(SYM, PercMKCaP, ClassBeta, Kmeans)]


### Clusters Hist DAWass #############
#We incorporate the Coefficient of Variation and Marketcap
Crypto.Famd.ext <- Crypto.Famd
Crypto.Famd.ext <- Crypto.Famd.ext[Crypto.Famd.ext$SYM %in% PLD.HQ$Ticker,]
Crypto.Famd.ext$CV <- Crypto.Famd.ext$volatility / Crypto.Famd.ext$rmedio
Crypto.Famd.ext <- merge(Crypto.Famd.ext,MKTCAPShare[,c(-3,-4)])

table(Crypto.Famd.ext$HistDAWass)
#Cluster 1
Crypto.Qualy.Cubic[PercMKCaP=='P90' & HistDAWass==1 & DecAge=='D6',.(SYM, PercMKCaP, PercVolume,
                                                       DecAge)]

#Cluster 3
Crypto.Qualy.Cubic[PercMKCaP=='P100' & DecAge=='D10',HistDAWass==3,.(SYM, PercMKCaP, PercVolume,
                                                  DecAge)]
temp <- Crypto.Qualy[Crypto.Qualy$SYM %in% PLD.HQ$Ticker,]
temp[PercMKCaP=='P100' & DecAge=='D10',DecRisk=='D4' & HistDAWass==3,
                   .(SYM, PercMKCaP, PercVolume,DecAge)]

### Clusters TADPOLE #############
Crypto.Qualy.Cubic[PercMKCap=='P100'&TADPole==1,.(SYM, ClassMKCap, PercMKCap, TADPole, ClassSharpeR)]


#### Intersections: Sub-groups  ####################################
#Descriptive stats
Crypto.Qualy.CubiCombi <- Crypto.Qualy.Cubic[Crypto.Qualy.Cubic$Combi<7,]

#Combi 2:
Crypto.Qualy.CubiCombi[Kmeans == 2 &HistDAWass==3 & TADPole == 2 & ClassSharpeR == 'Acc',
                       .(SYM, PercVolume, PercMKCaP, ClassSharpeR, Kmeans, HistDAWass, TADPole)]

Crypto.Qualy.CubiCombi[Kmeans == 2 &HistDAWass==3 & TADPole == 2 & ClassSharpeR == 'ERP',
                       .(SYM, PercVolume, PercMKCaP, ClassSharpeR, Kmeans, HistDAWass, TADPole)]

Crypto.Qualy.CubiCombi[Kmeans == 2 &HistDAWass==3 & TADPole == 2 & 
                              PercMKCaP =='P100' & ClassBeta == 'LowVol',
                       .(SYM, ClassBeta,PercVolume, PercMKCaP, Kmeans, HistDAWass, TADPole)]

#Combi 3:
Crypto.Qualy.CubiCombi[Kmeans == 3 &HistDAWass==3 & TADPole == 3 & PercVolume == 'P80',
             .(SYM, PercVolume, PercMKCaP, Kmeans, HistDAWass, TADPole)]

Crypto.Qualy.CubiCombi[Kmeans == 3 &HistDAWass==3 & TADPole == 3 & PercMKCaP == 'P90',
                       .(SYM, PercVolume, PercMKCaP, Kmeans, HistDAWass, TADPole)]

Crypto.Qualy.CubiCombi[Kmeans == 3 &HistDAWass==3 & TADPole == 3 & 
                               PercMKCaP =='P99' & ClassBeta == 'HighVol',
                       .(SYM, ClassBeta,PercVolume, PercMKCaP, Kmeans, HistDAWass, TADPole)]

#Combi 4:
Crypto.Qualy.CubiCombi[Kmeans == 3 &HistDAWass==3 & TADPole == 2 & PercMKCaP == 'P90',
                       .(SYM, PercVolume, PercMKCaP, Kmeans, HistDAWass, TADPole)]

Crypto.Qualy.CubiCombi[Kmeans == 3 &HistDAWass==3 & TADPole == 2 & 
                                PercMKCaP=='P99' & ClassSharpeR == 'ERP',
                       .(SYM, PercVolume, PercMKCaP, ClassSharpeR, Kmeans, HistDAWass, TADPole)]

Crypto.Qualy.CubiCombi[Kmeans == 3 &HistDAWass==3 & TADPole == 2 & 
                               PercMKCaP =='P99' & ClassBeta == 'HighVol',
                       .(SYM, ClassBeta,PercVolume, PercMKCaP, Kmeans, HistDAWass, TADPole)]


#Combi 1
Crypto.Qualy.CubiCombi[Kmeans == 2 &HistDAWass==3 & TADPole == 3 & PercVolume == 'P100',
                       .(SYM, PercVolume, PercMKCaP, Kmeans, HistDAWass, TADPole)]

#Combi 5
Crypto.Qualy.CubiCombi[Kmeans == 3 &HistDAWass==1 & TADPole == 3 & PercMKCaP == 'P70',
                       .(SYM, PercVolume, PercMKCaP, Kmeans, HistDAWass, TADPole)]

Crypto.Qualy.CubiCombi[Kmeans == 3 &HistDAWass==1 & TADPole == 3 & ClassBeta == 'NegBeta',
                       .(SYM, ClassBeta,PercVolume, PercMKCaP, Kmeans, HistDAWass, TADPole)]


#Combi 6
Crypto.Qualy.CubiCombi[Kmeans == 3 &HistDAWass==1 & TADPole == 2 & PercMKCaP == 'P70',
                       .(SYM, PercVolume, PercMKCaP, Kmeans, HistDAWass, TADPole)]

Crypto.Qualy.CubiCombi[Kmeans == 3 &HistDAWass==1 & TADPole == 2 & ClassBeta == 'NegBeta',
                       .(SYM, ClassBeta,PercVolume, PercMKCaP, Kmeans, HistDAWass, TADPole)]

Crypto.Qualy.CubiCombi[Kmeans == 3 &HistDAWass==1 & TADPole == 2 & DecAge == 'D4',
                       .(SYM, DecAge,PercVolume, PercMKCaP, Kmeans, HistDAWass, TADPole)]
