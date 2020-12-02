#install.packages("stargazer", dependencies = FALSE)
library("stargazer")
#install.packages("car")
library("car")
#install.packages("Rcpp")
library("Rcpp")
#install.packages("DescTools")
library(DescTools)
#install.packages("PseudoR2")

setwd("C:/Users/Anca/Dropbox/ICO_MASTER_PHD/FILE DA SISTEMARE")
DB<- read.csv2("ICO_pronte_analisi_10_genn_2019.csv")
copy=(DB[,-10])

str(copy)
table(copy$class)
#35  10 151 
table(copy$class2)
#45 151 
table(copy$class3)
#186   10 
copy$Neg_Bing_sc=scale(copy$Neg_Bing)
copy$Pos_Bing_sc=scale(copy$Pos_Bing)
copy$Sent_Bing_sc=scale(copy$Sent_Bing)

copy$Neg_NRC_sc=scale(copy$Neg_NRC)
copy$Pos_NRC_sc=scale(copy$Pos_Nrc)
copy$Sent_NRC_sc=scale(copy$Sent_Nrc)


### reg logistica
contrasts(copy$class2)
#######su
###f_s  0
###su   1
fit_log_Scam<-glm(class2 ~Oweb_dum + tw +
                    Paper_du+
                    adv_dum +Neg_Bing_sc+Pos_Bing_sc,
                  family="binomial", data=copy)
summary(fit_log_Scam)
step(fit_log_Scam)

fit_log_Scam1<-glm(class2 ~ tw+Paper_du+Pos_Bing_sc+adv_dum,
                   family="binomial", data=copy)
summary(fit_log_Scam1)

fit_log_Scam2<-glm(class2 ~ tw+Paper_du+Neg_Bing_sc+adv_dum,
                   family="binomial", data=copy)
summary(fit_log_Scam2)


fit_log_Scam3<-glm(class2 ~ tw+Paper_du+Pos_NRC_sc,
                   family="binomial", data=copy)
summary(fit_log_Scam3)

fit_log_Scam4<-glm(class2 ~ tw+Paper_du+Neg_NRC_sc,
                   family="binomial", data=copy)
summary(fit_log_Scam4)

fit_log_Scam5<-glm(class2 ~ tw+Paper_du+Sent_NRC_sc,
                   family="binomial", data=copy)
summary(fit_log_Scam5)

fit_log_Scam6<-glm(class2 ~ tw+Paper_du+Sent_NRC_sc,
                   family="binomial", data=copy)
summary(fit_log_Scam6)

fit_log_Scam7<-glm(class2 ~Oweb_dum + tw +
                     Paper_du+
                     nr_adv +nr_team+Sent_NRC_sc,
                   family="binomial", data=copy)
summary(fit_log_Scam7)
step(fit_log_Scam7)


#####modello inserito nel paper per review frontiers#############
fit_log_Scam8<-glm(class2 ~tw +
                     Paper_du+
                     nr_adv +nr_team+Sent_NRC_sc,
                   family="binomial", data=copy)
summary(fit_log_Scam8)
step(fit_log_Scam8)

###trials for roc###
fit <- fit_log_Scam8$fitted.values
hist(fit)

index <- sort.list(fit)
index[1:10]



hosmer <- matrix(c(copy$sta[index], fit[index]), byrow=F, nrow=196)
hosmer
observed <- rep(NA, 10)
for (i in 1:10) {observed[i] <- sum(hosmer[(20*(i-1)+1):(20*i),1])/20}

##################################à
#vif model paper  frontiers
library("rms")
car::vif(fit_log_Scam8)
viftable <- car::vif(fit_log_Scam8)
stargazer(viftable)


stargazer(fit_log_Scam8, title="Regression Results")


summary(fit_log_Scam8)
PseudoR2(fit_log_Scam8, which="McFaddenAdj")
PseudoR2(fit_log_Scam8, which="CoxSnell")
PseudoR2(fit_log_Scam8, which="all")
#full model # inserito in Appendix
fit_log_Scam10<-glm(class2 ~Oweb_dum + Code_dum + Telegram_du + tw + fb + ln + yt + 
                      gith + slack + reddit + btalk + ew + mm  + Neg_Bing_sc 
                    + Pos_Bing_sc  + Neg_NRC_sc + Pos_NRC_sc + Paper_du + 
                      nr_adv +nr_team , family="binomial", data=copy)
summary(fit_log_Scam10)
car::vif(fit_log_Scam10)

stargazer(fit_log_Scam10, title="Regression Results full model")

#senza sent e ridotto 
fit_log_Scam12<-glm(class2 ~Oweb_dum + Code_dum + Telegram_du + tw + fb + ln + yt + 
                      gith + slack + reddit + btalk + ew + mm  + Neg_Bing_sc 
                    + Pos_Bing_sc  + Neg_NRC_sc + Pos_NRC_sc + Paper_du + 
                      nr_adv +nr_team , family="binomial", data=copy)


summary(fit_log_Scam12)
z <- car::vif(fit_log_Scam12)
stargazer(z)

fit_log_Scam12<-glm(class2 ~Oweb_dum + Code_dum + Telegram_du + tw + fb + ln + yt + 
                      gith + slack + reddit + btalk + ew + mm  + Neg_Bing_sc 
                    + Pos_Bing_sc  + Neg_NRC_sc + Pos_NRC_sc + Paper_du + 
                      nr_adv +nr_team , family="binomial", data=copy)


summary(fit_log_Scam12)
car::vif(fit_log_Scam12)
###grouped by social
fit_log_Scam11<-glm(class2 ~Oweb_dum + Code_dum + Telegram_du + tw + fb + ln + yt + 
                      gith + slack + reddit + btalk + ew + mm, family="binomial", data=copy)


summary(fit_log_Scam11)
car::vif(fit_log_Scam11)


#install.packages('bgeva')
library(bgeva)

contrasts(copy$class3)

log_ex<-bgeva(class3 ~ Oweb_dum + tw + Paper_du + 
                Telegram_du + adv_dum +
                Pos_Bing_sc,
              data=copy)
summary(log_ex)


log_ex1<-bgeva(class3 ~ Oweb_dum + tw + Paper_du + 
                 Telegram_du + adv_dum +
                 Pos_NRC_sc,
               data=copy)
summary(log_ex1)


log_ex2<-bgeva(class3 ~  Oweb_dum + tw + Paper_du + 
                 Telegram_du + adv_dum +
                 Sent_NRC_sc, data=copy)
summary(log_ex2)
plot(log_ex1,scale=0,pages=1,shade=TRUE)

log_ex3<-bgeva(class3~ Oweb_dum + tw + Paper_du + 
                 Telegram_du + adv_dum +
                 Sent_Bing_sc, data=copy)
summary(log_ex3)

log_ex4<-bgeva(class3 ~ Oweb_dum + Telegram_du +
                 Sent_NRC_sc, data=copy)
summary(log_ex4)

log_ex5<-bgeva(class3 ~ Oweb_dum + tw +
                 Sent_NRC_sc+s(nr_adv), data=copy)
summary(log_ex5)



log_ex6<-bgeva(class3 ~ Oweb_dum  +
                 Sent_NRC_sc, data=copy)
summary(log_ex6)

log_ex7<-bgeva(class3 ~  Sent_NRC_sc, data=copy)
summary(log_ex7)


log_ex8<-bgeva(class3 ~  Sent_NRC_sc+s(nr_adv), data=copy)
summary(log_ex8)

log_ex9<-bgeva(class3 ~  Sent_NRC_sc+s(nr_adv)+s(nr_team), data=copy)
summary(log_ex9)

log_ex10<-bgeva(class3 ~  Sent_NRC_sc+s(nr_team), data=copy)
summary(log_ex10)

log_ex11<-bgeva(class3 ~ Oweb_dum+Telegram_du+Paper_du+tw+fb+
                  ln+yt+gith+reddit+btalk+s(nr_team)+adv_dum, data=copy)
summary(log_ex11)

log_ex12<-bgeva(class3 ~ Oweb_dum+fb
                , data=copy)
summary(log_ex12)

log_ex13<-bgeva(class3 ~ Oweb_dum+fb+Sent_NRC_sc
                , data=copy)
summary(log_ex13)

####MULTINOMIAL MODEL
require(foreign)
require(nnet)
require(ggplot2)
require(reshape2)


copy$class <- relevel(copy$class, ref = "sc")
mult_log_1<- multinom(class ~  Oweb_dum+Telegram_du+Paper_du+tw+fb+
                        ln+yt+gith+reddit+btalk+adv_dum+Sent_NRC_sc, data = copy)
summary(mult_log_1)


z <- summary(mult_log_1)$coefficients/summary(mult_log_1)$standard.errors
z

p <- (1 - pnorm(abs(z), 0, 1)) * 2
p

mult_log_2<- multinom(class ~ Oweb_dum+tw+
                        Sent_NRC_sc, data = copy)
summary(mult_log_2)

z2 <- summary(mult_log_2)$coefficients/summary(mult_log_2)$standard.errors
z2

p2 <- (1 - pnorm(abs(z2), 0, 1)) * 2
p2


copy$class <- relevel(copy$class, ref = "f")
mult_log_3<- multinom(class ~  Oweb_dum+Telegram_du+Paper_du+tw+fb+
                        ln+yt+gith+reddit+btalk+adv_dum+Sent_NRC_sc, data = copy)
summary(mult_log_3)


z3 <- summary(mult_log_3)$coefficients/summary(mult_log_3)$standard.errors
z3

p3 <- (1 - pnorm(abs(z3), 0, 1)) * 2
p3

copy$class <- relevel(copy$class, ref = "su")
mult_log_4<- multinom(class ~  Oweb_dum+Telegram_du+Paper_du+tw+fb+
                        ln+yt+gith+reddit+btalk+adv_dum+Sent_NRC_sc, data = copy)
summary(mult_log_4)


z4 <- summary(mult_log_4)$coefficients/summary(mult_log_4)$standard.errors
z4

p4 <- (1 - pnorm(abs(z4), 0, 1)) * 2
p4

mult_log_5<- multinom(class ~ Oweb_dum+tw+adv_dum+Paper_du+
                        Sent_NRC_sc, data = copy)
s5<-summary(mult_log_5)
s5
z5 <- summary(mult_log_5)$coefficients/summary(mult_log_5)$standard.errors
z5

p5 <- (1 - pnorm(abs(z5), 0, 1)) * 2
p5
s5
stargazer(s5)
stargazer(p5)
PseudoR2(mult_log_5, which="all")
#aggiunto VIF sulla multilogit 5
Vif_multilog <- car::vif(mult_log_5)
Vif_multilog

stargazer(Vif_multilog)

#senza tw---- inserita nel paper---fare vif e PSeudo

mult_log_6<- multinom(class ~ Oweb_dum+adv_dum+Paper_du+
                        Sent_NRC_sc, data = copy)
s6<-summary(mult_log_6)
s6
z6 <- summary(mult_log_6)$coefficients/summary(mult_log_6)$standard.errors
z6

p6 <- (1 - pnorm(abs(z6), 0, 1)) * 2
p6

####vif e pseudo inseriti nel paper. 
vif_multilog6 <- car::vif(mult_log_6)
vif_multilog6
stargazer(mult_log_6)
stargazer(s6)
stargazer(p6)
stargazer(vif_multilog6)
PseudoR2(mult_log_6, which="all")

#####senza oweb
mult_log_7<- multinom(class ~ tw+adv_dum+Paper_du+
                        Sent_NRC_sc, data = copy)
s7<-summary(mult_log_7)
s7
z7 <- summary(mult_log_7)$coefficients/summary(mult_log_7)$standard.errors
z7

p7 <- (1 - pnorm(abs(z7), 0, 1)) * 2
p7
s7
stargazer(s7)
stargazer(p7)
PseudoR2(mult_log_7, which="all")
#aggiunto VIF sulla multilogit 5
Vif_multilog7 <- car::vif(mult_log_7)
Vif_multilog7


#FULL MULTILOGIT # inserita nell' appendix 
mult_log_8<- multinom(class ~Oweb_dum + Code_dum + Telegram_du + tw + fb + ln + yt + 
      gith + slack + reddit + btalk + ew + mm  + Neg_Bing_sc 
    + Pos_Bing_sc  + Neg_NRC_sc + Pos_NRC_sc + Paper_du + 
      nr_adv +nr_team , data=copy)
s8<-summary(mult_log_8)
s8
z8 <- summary(mult_log_8)$coefficients/summary(mult_log_8)$standard.errors
z8

p8 <- (1 - pnorm(abs(z8), 0, 1)) * 2
p8
s8
stargazer(mult_log_8)
stargazer(p8)
PseudoR2(mult_log_8, which="all")
Vif_multilog8 <- car::vif(mult_log_8)
Vif_multilog8
stargazer(Vif_multilog8)
