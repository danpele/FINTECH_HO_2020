############################################################################################################
#############################################################################################################
######Code for: "Libra or Librae: Basket based stablecoins to mitigate foreign exchange volatility spillovers"
######Authors: Paolo Giudici, Thomas Leach and Paolo Pagnottoni
#############################################################################################################


#############################################################################################################
##Note. The code requires the use of the following file:
##-functions.R
#############################################################################################################


# Packages loading
# library(readr)
# library(PerformanceAnalytics)
# library(readxl)
# library(DescTools)
# library(NMOF)
library(Rsolnp)


#sourcing the file "functions.R"
source("functions.R")

# Set path to plotting
#path_to_plots <- "../../plots/"
path_to_data <- "data/"
# Path to exchange folder
path_to_exrates <- "data/Daily FXRates USD/"
x <- read.csv(paste0(path_to_data, "FX_Rates.csv")) # Check the number of obs

# Names
file_names <- list.files(paste0(path_to_exrates))
fx_file_names <- gsub(".*_", "", file_names)
fx_file_names <- gsub(" .*", "", fx_file_names)
fx_file_names <- c('USD', fx_file_names, 'SAC', 'SDR')

## Normalise
# # Get reduced values

RNVals <- normaliseSeries(x[nrow(x):1,c(2,3,4,6)])
colnames(RNVals) <- fx_file_names[c(1,2,3,4,6)]

# %% Optim Weights
Weights_all <- optimWeights(RNVals, init_vals = c(0.21, 0.14, 0.21, 0.21, 0.23))
Weights_SDR <-  c(0.42, 0.11, 0.31, 0.08, 0.08)

# Weights_SDR = Weights_SDR/norm(Weights_SDR)

#Get quantities from weights
Q_all <- Weights_all/as.numeric(c(1, x[1,c(2,3,4,6)]))
Q_SDR <- Weights_SDR/as.numeric(c(1, x[1,c(2,3,4,6)]))


# %% The Stable Currency
SAC <- rowSums(sweep(RNVals, MARGIN=2, Weights_all, "*"))
SDR <- rowSums(sweep(RNVals, MARGIN=2, Weights_SDR, "*"))

all_mat <- data.frame(RNVals, SAC)
all_mat <- all_mat[nrow(all_mat):1,]
cor_RNV <- round(cor(all_mat),2)

x_usd <- data.frame(rep(1,nrow(x)), 1/x[,c(2,3,4,6)])


USD_SAC <- rowSums(sweep(x_usd, MARGIN=2, Q_all, "*"))
USD_SDR <- rowSums(sweep(x_usd, MARGIN=2, Q_SDR, "*"))


# Get RNVALS with computed SDR / SAC values
all_mat_USD <- data.frame(x, USD_SAC = 1/USD_SAC, USD_SDR = 1/USD_SDR)
all_mat_USD <- all_mat_USD[nrow(all_mat_USD):1,]

RNVals_All <- normaliseSeries(all_mat_USD[,2:11])

# RNVals_All <- RNVals_All[nrow(RNVals_All):1,]

colnames(RNVals_All) <- fx_file_names

RNVals_All <- RNVals_All[nrow(RNVals_All):1,]


## Standard Deviations
beg=1559;
end=2342;

apply(RNVals_All, 2, sd, na.rm = TRUE) 
apply(RNVals_All[1:beg,], 2, sd, na.rm = TRUE) # Pre-crisis
apply(RNVals_All[beg:end,], 2, sd, na.rm = TRUE) # Crisis
apply(RNVals_All[end:nrow(RNVals_All),], 2, sd, na.rm = TRUE) # Post-crisis


