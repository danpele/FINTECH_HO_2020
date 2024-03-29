

# Financial Risk Meter (FRM) can be seen in action in these two applications:

# https://applications.firamis.de/frei/app/frm_app
# http://frm.wiwi.hu-berlin.de/




# clear all variables
rm(list = ls(all = TRUE))

library("quantmod")
library("quantreg")
library("doParallel")
library("foreach")


#####################################################################################################
##################################### Part 1 download data ##########################################
#####################################################################################################

##################################### Part 1.1 download 100 firms ###################################


companylist = read.csv("companylist 2016.csv")

# companylist2016 - downloaded from this source
# http://www.nasdaq.com/screening/companies-by-industry.aspx?industry=Finance&sortname=country&sorttype=1

# Array with firm names sorterd by market capitalization
firm_names = as.character(companylist[, 1])

# # starting date for 100 firms
# a = "2007-12-28"
# # starting date for macro variables, 1 day lag in data retrieval
# a_m = "2007-12-29"
# # date for 100 firms
# b = "2019-03-28"
# # date for macro variables, 1 day lag
# b_m = "2019-03-29"

# # starting date for 100 firms
# a = "2018-12-03"
# # starting date for macro variables, 1 day lag in data retrieval
# a_m = "2018-12-04"
# # date for 100 firms
# b = "2020-03-27"
# # date for macro variables, 1 day lag
# b_m = "2020-03-28"

# starting date for 100 firms
a = "2007-12-28"
# starting date for macro variables, 1 day lag in data retrieval
a_m = "2007-12-29"
# date for 100 firms
b = "2020-03-27"
# date for macro variables, 1 day lag
b_m = "2020-03-28"

frm_data_download = function(firm_names, a, b, a_m, b_m) {
  # calls Yahoo Finance through internet
  example = getSymbols(firm_names[1], src = "yahoo", from = a, to = b, auto.assign = FALSE)
  example_time = as.matrix(example[-1, 1])
  time_points = nrow(as.data.frame(example))
  
  # setting parameters for the loop number of firms
  n = length(firm_names)
  # counter (initial value is 1)
  s = 1
  # number of firms used
  max_num = 100
  
  # creating initial matrix for all firms and all time points
  firms_closed_price = matrix(0, time_points, n)
  colnames(firms_closed_price) = firm_names
  
  # sometimes data for a company can not be captured (bad internet connection,
  # problems with correct name) in this case extend this list the list can vary
  # from time to time
  bad_list = c("STI","OZM","TMK","BBT","ZIONZ", "SNFCA", "KMPA", "CATYW", "MBFIP", "HAWKB", "JLL", "SYA", 
               "NPBC", "MHFI", "SFG", "FNFV", "FNFG", "FMER", "BBCN", "SFN", "WIBC", "RSE",
               "CBG", "AFSI", "OZRK", "PVTB", "PB","SYF","KKR","CFG","ALLY","CIT","VOYA","OAK",
               "APO","RGA","CBOE","TRU","RLGY","CG","NAVI","LPLA","FAF","LC","BKU","DNB","VIRT")
  
  # Main loop : data from yahoo to firms_closed_price
  for (i in 1:n) {
    # 1) Check whether contained in bad list, if then skip
    if (firm_names[i] %in% bad_list) {
      next
    }
    # 2) check whether the complete time series is available, if not skip and print
    # name
    prices = getSymbols(firm_names[i], src = "yahoo", from = a, to = b, auto.assign = FALSE)
    if (nrow(prices) != time_points) {
      # skip this firm if different length of time points
      print(firm_names[i])
      next
    }
    # 3) get data and save in firms_closed_price
    prices_data = as.data.frame(prices)
    firms_closed_price[, i] = as.matrix(prices_data[, 6])
    print(s)
    # increase counter
    s = s + 1
    # 4) stop if max_num achieved
    if (s > max_num) 
      break
  }
  
  # checking which firms were filled
  cs = colSums(firms_closed_price)
  # sum over daily observations to check for zero columns number of firms for which
  # the sum is larger than 0
  length(cs[cs > 0])
  
  # taking submatrix from firms_closed_price with 200 companies
  collected_firms = names(cs[cs > 0])
  data_firms = firms_closed_price[, collected_firms]
  
  # transform the company_prices_200 into log returns
  returns_final = diff(log(data_firms))
  rownames(returns_final) = rownames(example_time)
  
  ################################## Part 1.2 download macro variables ############################
  macro_names = c("^VIX", "^GSPC", "IYR", "DGS3MO", "DGS10", "DBAA")
  
  # Part 2.1: download VIX, GSPC (S&P500) and IYR (iShares Dow Jones US Real
  # Estate) from yahoo finance
  VIX = as.matrix(getSymbols(macro_names[1], src = "yahoo", from = a_m, to = b_m, 
                             auto.assign = FALSE)[, 6])
  GSPC = as.matrix(getSymbols(macro_names[2], src = "yahoo", from = a_m, to = b_m, 
                              auto.assign = FALSE)[, 6])
  IYR = as.matrix(getSymbols(macro_names[3], src = "yahoo", from = a_m, to = b_m, 
                             auto.assign = FALSE)[, 6])
  
  # data of first three variables
  data_ft = as.matrix(cbind(VIX, GSPC, IYR))
  # transform GSPC and IYR into log returns without VIX
  returns_m = diff(log(data_ft[, -1]))
  First_three_macro_in = cbind(data_ft[-1, 1], returns_m)
  First_three_macro = First_three_macro_in
  # remove last row
  #First_three_macro = First_three_macro_in[-nrow(First_three_macro_in), ]
  
  # Part 2.2: download the other 3 macro from Federal reserve Bank measure the
  # length of first three macro variables, so that the last three variables have
  # the same length with them.
  c = nrow(First_three_macro)
  # 3 month Treasury change download 3 month Treasury maturities, from 20150713 can
  # not use getSymbol to download from Federal any more, only can use the following
  # links.
  ThreeMT = as.numeric(as.matrix(read.csv("https://research.stlouisfed.org/fred2/series/DGS3MO/downloaddata/DGS3MO.csv", 
                                          na.strings = ".")[, -1]))
  True_ThreeMT = na.omit(ThreeMT)
  lt = as.matrix(True_ThreeMT)
  # set the length of this variable the same as the first three variables, take
  # last c values
  output_ThreeMT = as.matrix(lt[(length(lt) - c):length(lt), 1])
  # calculate the 3 month Treasury change
  change_ThreeMT = diff(output_ThreeMT)
  
  # Slope of yield curve download 10 year Treasury maturities
  Tenyield = as.numeric(as.matrix(read.csv("https://research.stlouisfed.org/fred2/series/DGS10/downloaddata/DGS10.csv", 
                                           na.strings = ".")[, -1]))
  True_Tenyield = na.omit(Tenyield)
  lyc = as.matrix(True_Tenyield)
  output_Tenyield = as.matrix(lyc[(length(lyc) - c):length(lyc), 1])
  # calculate Slope of yield curve
  slope_yield = as.matrix(output_Tenyield - output_ThreeMT)[-1]
  
  # credit spread download BAA
  DayBAA = as.numeric(as.matrix(read.csv("https://research.stlouisfed.org/fred2/series/DBAA/downloaddata/DBAA.csv", 
                                         na.strings = ".")[, -1]))
  True_DayBAA = na.omit(DayBAA)
  lc = as.matrix(True_DayBAA)
  output_True_DayBAA = as.matrix(lc[(length(lc) - c):length(lc), 1])
  # calculate credit spread
  credit_spread = as.matrix(output_True_DayBAA - output_Tenyield)[-1]
  
  # combine all the macro variables
  rest_three_macro = cbind(change_ThreeMT, slope_yield, credit_spread)
  six_macro = cbind(First_three_macro, rest_three_macro)
  
  # scale variables to [0,1]
  scale_macro = six_macro
  nnrow = nrow(scale_macro)
  nncol = ncol(scale_macro)
  m = matrix(0, nnrow, nncol)
  for (i in 1:nncol) {
    m[, i] = (scale_macro[, i] - min(scale_macro[, i]))/(max(scale_macro[, i]) - 
                                                           min(scale_macro[, i]))
  }
  colnames(m) = c("^VIX", "^GSPC", "IYR", "3MTCM", "Yield", "Credit")
  
  ####################### Part 1.3 combine 100 firms and 6 macro variables ########################
  firms_data = returns_final
  macro_data = m
  full_data = cbind(firms_data, macro_data)
  full_data = round(full_data, digits = 9)
  Date_wf = strptime(as.character(rownames(full_data)), "%Y-%m-%d")
  Date_rf = format(Date_wf, "%d/%m/%Y")
  Date = as.data.frame(Date_rf)
  names(Date) = "Date"
  rownames(full_data) = NULL
  final_data = cbind(Date, full_data)
  finalresults = list()
  finalresults$data = final_data
  return(finalresults)
}
frm_dd = try(frm_data_download(firm_names, a, b, a_m, b_m))

final_data = frm_dd$data
dim(final_data)

#####################################################################################################
####################################### Part 2 calculate FRM ########################################
#####################################################################################################

source("FRM_qrL1.r")
source("quantilelasso.r")
detectCores()
registerDoParallel(cores = 2)
getDoParWorkers()

### set the number of working days needed to be calculate
wd = 30 #3000 #60 #2700
### set number of firms involved in the calculation
nf = 100

x0 = final_data
dt = final_data[, 1][(length(final_data[, 1]) - wd + 1):length(final_data[, 1])]
dt = as.Date(dt, format = "%d/%m/%Y")

# 6 macro state variables
m = as.matrix(x0[, (ncol(x0) - 5):ncol(x0)])
# log returns of 100 firms
xx0 = as.matrix(x0[, 2:(ncol(x0) - 6)])
# start the linear quantile lasso estimation for each firm number of rows of log
# return
n = nrow(xx0)
# number of covariates
p = ncol(xx0) + ncol(m) - 1
# quantile level
tau = 0.05
# moving window size
ws = 63
ptm <- proc.time()
lambda = foreach(k = 1:nf, .combine = cbind) %:% foreach(i = ((n - ws) - wd + 1):(n - ws)) %dopar% {
  print(i)
  # log return of firm k
  y = as.matrix(xx0[, k])
  # log returns of firms except firm k
  xx1 = as.matrix(xx0[, -k])
  yw = y[i:(i + ws)]
  mw = m[i:(i + ws), ]
  xx = xx1[i:(i + ws), ]
  ## all the independent variables
  xxw = cbind(xx, mw)
  fit = linear(yw, xxw, tau, i, k)
  print(fit$lambda.in)
}
proc.time() - ptm
lambda_all = matrix(unlist(lambda), ncol = nf, byrow = FALSE)
FRM = apply(lambda_all, 1, mean)
FRM

### combine the old FRM data and the updated data
dt=as.data.frame(dt)
Final_FRM = cbind(dt,FRM)
colnames(Final_FRM) = c("date", "FRM")
write.csv(format(Final_FRM, scientific = FALSE), file = paste("lambda_mean_206vars_", 
                                                              b, ".csv", sep = ""), row.names = FALSE, quote = FALSE)
plot(Final_FRM,type = "l",col="blue")
### show the current risk level
maxFRM = max(Final_FRM[, 2])
curFRM = Final_FRM[, 2][length(Final_FRM[, 2])]
ql = curFRM/maxFRM
print(ql)
