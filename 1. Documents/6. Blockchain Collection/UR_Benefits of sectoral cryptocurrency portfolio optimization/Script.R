
library(xts)
library(bindr)
library(PerformanceAnalytics)
library(PortfolioAnalytics)
library(ROI)
library(Rglpk)
library(quadprog)
library(ROI.plugin.quadprog)
library(ROI.plugin.glpk)



######## LOADING THE DATA FROM THE WORKING DIRECTORY ########
load("data.RData")


######## DEFINING 50 CRYPTO DATAFRAME BY CMC. TO USE ALL CRYPTO DON'T RUN THIS ######## 
DATASET <- DATASET[c(1:51)]


####### DEFINING TIME SERIES AND DISCRETE RETURN  ######## 
DATASET.XTS <- xts(DATASET[,-1], order.by=DATASET[,1])
R.DATASET = Return.calculate(xts(DATASET.XTS), method="discrete")
R.DATASET <- na.omit(R.DATASET)
CRYPTO <- colnames(R.DATASET)
CRYPTO


######## CREATE PORTFOLIO OBJECT ########
pspec <- portfolio.spec(assets=CRYPTO)


########### ADD CONSTRAINTS TO THE PORTFOLIO OBJECT WITHOUT SECTORIAL CRYPTO. RUN THIS ONLY FOR 50 CRYPTO ##################
pspec <- add.constraint(portfolio=pspec, type="weight_sum", min_sum=1, max_sum=1)
pspec <- add.constraint(portfolio=pspec, type="box", min=c(0), max=c(1))


########### ADD CONSTRAINTS TO THE PORTFOLIO OBJECT WITH SECTORIAL CRYPTO. RUN THIS ONLY FOR ALL CRYPTO ##################
pspec <- add.constraint(portfolio=pspec, type="weight_sum", min_sum=1, max_sum=1)
pspec <- add.constraint(portfolio=pspec, type="group", 
                              groups=list(groupA=c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50), 
                                          groupB=c(51,52,53,54,55,56,57,58,59,60,61,62,63,64,65)),
                              group_min=c(0.8, 0.2), group_max=c(0.81, 0.21))
pspec <- add.constraint(portfolio=pspec, type="box", min=c(0), max=c(1))



######## ADD OBJECTIVE TO MINIMIZE VARIANCE ######## 
portf_minvar <- add.objective(portfolio=pspec, type="risk", name="var")
print(portf_minvar)
summary(portf_minvar)


######## ADD OBJECTIVE TO MINIMIZE CVaR ########
portf_minCVaR <- add.objective(portfolio=pspec, type="risk", name="ETL")
print(portf_minCVaR)
summary(portf_minCVaR)


######## ADD OBJECTIVE TO MAXIMIZING SHARPE RATIO - STDEV ########
portf_maxSR <- add.objective(portfolio=pspec, type="return", name="mean")
portf_maxSR <- add.objective(portfolio=portf_maxSR, type="risk", name="var")
print(portf_maxSR)
summary(portf_maxSR)


######## ADD OBJECTIVE TO MAXIMIZING STARR RATIO - CVaR ########
portf_maxSTARR <- add.objective(portfolio=pspec, type="return", name="mean")
portf_maxSTARR <- add.objective(portfolio=portf_maxSTARR, type="risk", name="ETL", arguments = list(p=0.95))
print(portf_maxSTARR)
summary(portf_maxSTARR)


######## ADD OBJECTIVE TO MAXIMIZING UTILITY ########
portf_maxUT <- add.objective(portfolio=pspec, type="return", name="mean")
portf_maxUT <- add.objective(portfolio=portf_maxUT, type="risk", name="var", risk_aversion=1)
print(portf_maxUT)
summary(portf_maxUT)


######## ADD OBJECTIVE TO MAXIMIZING MEAN RETURN ########
portf_maxret <- add.objective(portfolio=pspec, type="return", name="mean")
print(portf_maxret)
summary(portf_maxret)



############### BACKTESTING ###############

MinimumVarianceBT <- optimize.portfolio.rebalancing(R=R.DATASET,portf_minvar,trace=TRUE, optimize_method="random", rebalance_on = "months", training_period = 10)
print(MinimumVarianceBT)

MinimumCVaRBT <- optimize.portfolio.rebalancing(R=R.DATASET,portf_minCVaR, trace=TRUE, optimize_method="ROI", rebalance_on = "months", training_period = 10)
print(MinimumCVaRBT)

maxSRVarianceBT <- optimize.portfolio.rebalancing(R=R.DATASET,portf_maxSR, maxSR=TRUE, trace=TRUE, optimize_method="random", rebalance_on = 'months', training_period = 10)
print(maxSRVarianceBT)

maxSTARRBT <- optimize.portfolio.rebalancing(R=R.DATASET,portf_maxSTARR, maxSTARR=TRUE, trace=TRUE, optimize_method="ROI", rebalance_on = 'months', training_period = 10)
print(maxSTARRBT)

maxUTBT <- optimize.portfolio.rebalancing(R=R.DATASET,portf_maxUT, trace=TRUE, optimize_method="random", rebalance_on = 'months', training_period = 10)
print(maxUTBT)

MeanVarianceBT <- optimize.portfolio.rebalancing(R=R.DATASET,portf_maxret, trace=TRUE, optimize_method="ROI", rebalance_on = 'months', training_period = 10)
print(MeanVarianceBT)



######## EXTRACTING PORTFOLIO RETURNS ########

MinVariancePortfReturns=Return.rebalancing(R=R.DATASET, weights=extractWeights(MinimumVarianceBT))
colnames(MinVariancePortfReturns) = c('MinVar')

MinCVaRPortfReturns=Return.rebalancing(R=R.DATASET, weights=extractWeights(MinimumCVaRBT))
colnames(MinCVaRPortfReturns) = c('MinCVaR')

maxSRVariancePortfReturns=Return.rebalancing(R=R.DATASET, weights=extractWeights(maxSRVarianceBT))
colnames(maxSRVariancePortfReturns) = c('MaxSR')

maxSTARRPortfReturns=Return.rebalancing(R=R.DATASET, weights=extractWeights(maxSTARRBT))
colnames(maxSTARRPortfReturns) = c('MaxSTARR')

maxUTPortfReturns=Return.rebalancing(R=R.DATASET, weights=extractWeights(maxUTBT))
colnames(maxUTPortfReturns) = c('MaxUT')

MeanVariancePortfReturns=Return.rebalancing(R=R.DATASET, weights=extractWeights(MeanVarianceBT))
colnames(MeanVariancePortfReturns) = c('MaxMean')


####### DEFINING TIME SERIES AND DISCRETE RETURN FOR CRIX  ######## 
CRIX.xts <- xts(CRIX[, -1], order.by=as.Date(CRIX$date))
R.CRIX = Return.calculate(xts(CRIX.xts), method="discrete")
R.CRIX <- na.omit(R.CRIX)
names(R.CRIX) <- c("CRIX")


######## MERGING ALL IN ONE XTS FRAME ########
PortfolioComparisonData <- merge.xts(MinVariancePortfReturns,
                                     MinCVaRPortfReturns,
                                     maxSRVariancePortfReturns,
                                     maxSTARRPortfReturns,
                                     maxUTPortfReturns,
                                     MeanVariancePortfReturns,
                                     R.CRIX)

PortfolioComparisonData <- na.omit(PortfolioComparisonData)


language <- "English"
Sys.setlocale("LC_TIME", language)

#### GRAPHS ####
charts.PerformanceSummary(PortfolioComparisonData, main = "Performance summary", colorset=rich6equal, lwd=2, ylog=TRUE)


######## PERFORMANCE METRICS - REGRESSION - INFORMATIONM RATIO - TREYNOR RATIO ########
table.CAPM(PortfolioComparisonData[,1:7], PortfolioComparisonData[,7])[c(2,6,11,12),]

######## GEOMETRIC RETURN - STD DEV - SHARPE ########
table.AnnualizedReturns(PortfolioComparisonData, digits = 4)

######## MAX DRAWDOWN ########
maxDrawdown(PortfolioComparisonData)

######## CUMULATIVE RETURN ########
CUMULATIVE <- cumprod(1+PortfolioComparisonData)
tail(CUMULATIVE, 1)

######## RISK ADJUSTED RETURN: MSquared #########
print(MSquared(PortfolioComparisonData[,1], PortfolioComparisonData[,7]), digits = 4)

######## JENSEN'S ALPHA #########
print(CAPM.jensenAlpha(PortfolioComparisonData[,1], PortfolioComparisonData[,7]), digits = 4)

