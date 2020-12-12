#0. Preparation
rm(list = ls(all = TRUE))
graphics.off()

library(ggplot2)
library(data.table)
library(igraph)
require(timeDate)
library(stringr)
library(graphics)
library(magick)

wdir = "/Please change the folder path here"
setwd(wdir)

source("FRM_Statistics_Algorithm.R")

output_path = paste0("Output/")

#----------------------------------------Data preparation ----------------------------------------

Date_Start_Data = 20190101 #Data source
Date_End_Data   = 20200803
Date_Start      = 20200801 #Output
Date_End        = 20200803

#Estimation Window Size, s = 63
s   = 63      
#Tail Risk Level, tau = 0.05
tau = 0.05   
#Number of Iterations, I = 20
I   = 20  
#Number of cryptos
J   = 15
#CoStress top K 
K   = 10


## 1. Data Preprocess
Crypto_Prices = read.csv(file = paste0("Crypto_Price_", Date_End_Data, ".csv"), header = TRUE)
Mktcap        = read.csv(file = paste0("Crypto_Mktcap_", Date_End_Data, ".csv"), header = TRUE)
Macro         = read.csv(file = paste0("Crypto_Macro_", Date_End_Data, ".csv"), header = TRUE)

colnames(Mktcap) == colnames(Crypto_Prices)

M_crypto = ncol(Mktcap) - 1
M_macro  = ncol(Macro) - 1
M        = M_crypto+M_macro

colnames(Mktcap)[1]        = "date"
colnames(Crypto_Prices)[1] = "date"
colnames(Macro)[1]         = "date"

#Load the stock prices and macro-prudential data matrix
All_prices           = merge(Crypto_Prices, Macro, by = "date", all.x = TRUE)
Ticker_str           = as.data.frame(All_prices$date[-1])
colnames(Ticker_str) = "date"
Ticker               = as.numeric(gsub("-", "", Ticker_str$date))
N                    = nrow(Ticker_str)
 
#Align Mktcap rows to All_return
Mktcap                = merge(Ticker_str, Mktcap, by = "date", all.x = TRUE)
Mktcap[is.na(Mktcap)] = 0
All_prices$BV         = exp(All_prices$BV)
Mktcap$date == Ticker_str$date

#Calculate the daily return matrix of all selected financial companies and macro-prudential variables; 
All_return                    = diff(log(as.matrix(All_prices[, -1])))
All_return[is.na(All_return)] = 0
Crypto_return                 = All_return[, 1:M_crypto]
Macro_return                  = All_return[, (M_crypto + 1):M]

#Sorting the Market Capitalization Data
FRM_Sort = function(Data){sort(Data, decreasing = TRUE, index.return = TRUE)}
Mktcap_Index = matrix(0, N, M_crypto)
Mktcap_Sort  = apply(Mktcap[, -1], 1, FRM_Sort)
for (t in 1:N) Mktcap_Index[t,] = Mktcap_Sort[[t]]$ix
Mktcap_Index = cbind(Ticker, Mktcap_Index)


#----------------------------------------FRM Calculation----------------------------------------

## 2. Estimation

#Row index corresponding to Date_Start and Date_End
N0 = which(Ticker == Date_Start)
N1 = which(Ticker == Date_End)
  
Adj_matix       = matrix(0, J, J + M_macro) 
FRM_series      = matrix(0, N1 - N0 + 1, J + 1)
FRM_series[, 1] = Ticker[N0:N1]

#Select the biggest cryptos 
Biggest_Index = as.matrix(Mktcap_Index[N0, 2:(J + 1)])

for (t in N0:N1){ 
  Data = cbind(Crypto_return[(t - s + 1):t, Biggest_Index], Macro_return[(t - s):(t - 1),])
  for (j in 1:J){ 
    #FRM_Quantile_Regression
    Est = FRM_Quantile_Regression(as.matrix(Data), j, tau, I)
    #Lambda distribution
    Est_lambda_hat = abs(data.matrix(Est$lambda[which(Est$Cgacv == min(Est$Cgacv))]))
    FRM_series[t - N0 + 1, j + 1] = Est_lambda_hat         
    #Adjacency matrices
    Est_hat = t(as.matrix(Est$beta[which(Est$Cgacv == min(Est$Cgacv)),]))
    if (j == 1) {
      Adj_matix[j, 2:(J + M_macro)] = Est_hat} 
    else {
      Adj_matix[j, c(1:(j - 1), (j + 1):(J + M_macro))] = Est_hat}
  }
  Adj_matix_final = Adj_matix[, 1:J]
  colnames(Adj_matix_final) = colnames(Data)[1:J]
  rownames(Adj_matix_final) = colnames(Data)[1:J]
  write.csv(Adj_matix_final, paste0(output_path, "/Adj_Matrices/Adj_Matix_", Ticker[t], ".csv"), quote = FALSE) 
}

#-----------------------------------------Output-----------------------------------------

## 3. Updated FRM index
FRM_index = data.frame(Date = as.character(Ticker_str$date[N0:N1]), FRM = round(rowSums(FRM_series[, 2:(J + 1)])/J, digits = 8))
write.csv(FRM_index, paste0(output_path, "/FRM_index.csv"), row.names = FALSE, quote = FALSE)


## 4. Lambda for each crypto
colnames(FRM_series) = c(as.character("Date"), colnames(Data)[1:J])
write.csv(FRM_series, paste0(output_path, "/lambdas_", Date_Start, "_", Date_End, ".csv"), row.names = FALSE, quote = FALSE) 


## 5.  Top ten risky cryptos with highest lambda on last day
top10 = cbind(colnames(Data)[order(FRM_series[nrow(FRM_series),c(2:(J + 1))], decreasing = TRUE)[1:K]],sort(FRM_series[nrow(FRM_series),c(2:(J + 1))],decreasing = TRUE)[1:K])
colnames(top10) = c('Crypto','Risk')
write.csv(top10, paste0(output_path, "/Top10_Risky_Cryptos_",Date_End, ".csv"), quote = FALSE,row.names = FALSE)


## 6. Boxplot
df = data.frame(x = as.factor(rep(FRM_series[, 1], J)), y = c(FRM_series[, -1]))

png(paste0(output_path, "/boxplot_", Date_Start, "_", Date_End, ".png"), width = 900, height = 600, bg = "transparent")
p = ggplot(df, aes(x = x, y = y)) +  
  geom_boxplot() + 
  stat_summary(fun = max, geom = "line", aes(group = 1), colour = "red")  + 
  stat_summary(fun = max, geom = "point", aes(group = 1), colour = "red") +
  stat_summary(fun = mean, geom = "line", aes(group = 1), colour = "blue") +
  stat_summary(fun = mean, geom = "point", colour = "blue") +
  theme(panel.grid = element_blank(), 
        axis.title = element_blank(),
        panel.background = element_rect(fill = "transparent", colour = NA),
        plot.background = element_rect(fill = "transparent", colour = NA),
        legend.box.background = element_rect(fill = "transparent"),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 90, hjust = 1, size = 12),
        axis.text.y = element_text(size = 14))
print(p)
dev.off()


## 7. Movie
main_node = "BTC"
date      = gsub("-", "", FRM_index$Date)

fig = image_graph(width = 1000, height = 1000, res = 96, bg="transparent")
for(t in 1:(nrow(FRM_index))){
  adj0 = read.csv(file=paste0("Output/Adj_Matrices/Adj_Matix_", date[t], ".csv"), header = TRUE, sep = ",", row.names = 1)
  adj0 = as.matrix(adj0)[1:J, 1:J] 
  adj0 = apply(adj0, 2, as.numeric)
  netw1 = graph_from_adjacency_matrix(adj0, mode = "directed", weighted = T)
  V(netw1)$color = ifelse(V(netw1)$name == main_node, "orange", "lightgrey")
  colors = rep("Gray", alpha.f = .8, length(E(netw1)))
  colors = ifelse(head_of(netw1, E(netw1))$name == main_node, 'blue', colors) #inflow
  colors = ifelse(tail_of(netw1, E(netw1))$name == main_node, 'orange', colors) #outflow
  plot(netw1, layout = layout_in_circle, vertex.label = colnames(adj0), edge.width = 0.8, 
       edge.color = colors, edge.arrow.size = 0.9, edge.arrow.width = 1, bg="transparent",
       vertex.size = 700*FRM_series[t, -1])
  title(xlab = date[t], sub = paste0("FRM: ", round(FRM_index$FRM[t], 5)), cex.lab = 1.15, 
        font.lab = 2, cex.sub = 1.15, font.sub = 2)
}
dev.off()
animation = image_animate(fig, fps = 5)
image_write(animation, paste0(output_path, "/", "Movie_", date[1], "_", date[t], ".gif"))









