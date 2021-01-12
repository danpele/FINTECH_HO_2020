#Alluvial Graph
# https://cran.r-project.org/web/packages/alluvial/vignettes/alluvial.html

#Librerias específicas
libs <- c('dplyr', 'stringr', 'forcats',     # wrangling
          'knitr','kableExtra',               # table styling
          'ggplot2','alluvial','ggalluvial',  # plots
          'nycflights13')                     # data
invisible(lapply(libs, library, character.only = TRUE))

#Datos
#DATASET
setwd('D:/DOCTORADO/Scripts/R/Revision1/')
nfich2 <- c("Dataset/QualyQuantRev1.RData")
load(nfich2)

QualyAlluv <- Crypto.Qualy[,.(Kmeans,HistDAWass,TADPole)]
QualyAlluv$KM3_ <- Crypto.Qualy$Kmeans

TADP <- Crypto.Qualy %>% 
  count(TADPole) %>% 
  pull(TADPole)

HIST <- Crypto.Qualy %>% 
  count(HistDAWass) %>% 
  pull(HistDAWass)

KMEA_ <- Crypto.Qualy %>%
  count(Kmeans) %>%
  pull(Kmeans)

KMEA <- QualyAlluv %>% 
  count(Kmeans, HistDAWass, TADPole, KM3_) %>%
  mutate(Kmeans = fct_relevel(as.factor(Kmeans),c("1","2","3")))

alluvial(KMEA %>% select(-n),
         freq = KMEA$n, border = NA, alpha =0.5,
         col=case_when(KMEA$Kmeans == "1" ~ "blue",
                       KMEA$Kmeans == "2" ~ "red",
                       TRUE ~ "orange"),
         cex = 0.75,
         axis_labels = c("Kmeans", "HistDAWass","TADPole","Kmeans_"),
         hide = KMEA$n < 1)

QualyAlluv <- Crypto.Qualy[,.(KM3,HistKmean,TADPole3)]
QualyAlluv$KM3_ <- Crypto.Qualy$KM3
