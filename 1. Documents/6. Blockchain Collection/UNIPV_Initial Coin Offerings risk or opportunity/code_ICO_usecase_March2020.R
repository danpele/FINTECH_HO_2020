#####packages

install.packages("stargazer", dependencies = FALSE)
library("stargazer")
install.packages("car")
library("car")
install.packages("Rcpp")
library("Rcpp")
install.packages("DescTools")
library(DescTools)
install.packages("PseudoR2")
library('PseudoR2')
install.packages('rms')
library("rms")

require(foreign)
require(nnet)
require(ggplot2)
require(reshape2)

#####################
### PATH

setwd("C:/Users/Paola Cerchiello/Downloads")

##### IMPORT DATASET
DB<- read.csv2("ICO_use_Case_March2020.csv")
copy=DB

str(copy)###structure of the dataset

#### TARGET VARIABLES
table(copy$class)
#35  10 151 
table(copy$class2)
#45 151 
table(copy$class3)
#186   10 

####SCALING sentiment VARIABLES
copy$Neg_Bing_sc=scale(copy$Neg_Bing)
copy$Pos_Bing_sc=scale(copy$Pos_Bing)
copy$Sent_Bing_sc=scale(copy$Sent_Bing)

copy$Neg_NRC_sc=scale(copy$Neg_NRC)
copy$Pos_NRC_sc=scale(copy$Pos_Nrc)
copy$Sent_NRC_sc=scale(copy$Sent_Nrc)


### logistic reg 
contrasts(copy$class2)##target variable specification
#######su
###f_s  0
###su   1


fit_log_Scam<-glm(class2 ~tw +
                     Paper_du+
                     nr_adv +nr_team+Sent_NRC_sc,
                   family="binomial", data=copy)

summary(fit_log_Scam) ## model summary
step(fit_log_Scam) ##stepwise selection



##################################à
#vif model and goodness of fit

car::vif(fit_log_Scam)
viftable <- car::vif(fit_log_Scam)

PseudoR2(fit_log_Scam, which="McFaddenAdj")
PseudoR2(fit_log_Scam, which="CoxSnell")
PseudoR2(fit_log_Scam, which="all")



#full model # available in Appendix
fit_log_Scam1<-glm(class2 ~Oweb_dum + Code_dum + Telegram_du + tw + fb + ln + yt + 
                      gith + slack + reddit + btalk + ew + mm  + Neg_Bing_sc 
                    + Pos_Bing_sc  + Neg_NRC_sc + Pos_NRC_sc + Paper_du + 
                      nr_adv +nr_team , family="binomial", data=copy)
summary(fit_log_Scam1)
car::vif(fit_log_Scam1)


####MULTINOMIAL MODEL
##TARGET VARIABLE SPECIFICS
contrasts(copy$class)

copy$class <- relevel(copy$class, ref = "su")


#MULTILOGIT MODEL FITTED

##PARAMETER ESTIMATES
mult_log<- multinom(class ~ Oweb_dum+adv_dum+Paper_du+
                        Sent_NRC_sc, data = copy)
s<-summary(mult_log)
s

##CALCULATE STANDARD ERRORS
z <- summary(mult_log)$coefficients/summary(mult_log)$standard.errors
z

###CALCULATE PVALUES
p <- (1 - pnorm(abs(z), 0, 1)) * 2
p

###goodness of fit and VIF index
PseudoR2(mult_log, which="all")
vif_multilog<- car::vif(mult_log)
vif_multilog



#FULL MULTILOGIT FROM THE APPENDIX

##PARAMETER ESTIMATES
mult_log_1<- multinom(class ~Oweb_dum + Code_dum + Telegram_du + tw + fb + ln + yt + 
      gith + slack + reddit + btalk + ew + mm  + Neg_Bing_sc 
    + Pos_Bing_sc  + Neg_NRC_sc + Pos_NRC_sc + Paper_du + 
      nr_adv +nr_team , data=copy)
s1<-summary(mult_log_1)
s1

##CALCULATE STANDARD ERRORS
z1 <- summary(mult_log_1)$coefficients/summary(mult_log_1)$standard.errors
z1

###CALCULATE P-VALUES
p1 <- (1 - pnorm(abs(z1), 0, 1)) * 2
p1

###goodness of fit and VIF index
PseudoR2(mult_log_1, which="all")
Vif_multilog1 <- car::vif(mult_log_1)
Vif_multilog1

