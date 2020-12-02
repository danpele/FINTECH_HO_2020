### Functions ###
library(Rsolnp)

# read
read_dta <- function(path_to_data){
  
  file_names <- list.files(paste0(path_to_data))
  # file_names <- {files.name};
  
  priceMat <- list()
  #% priceMat = zeros(n, length(file_names));
  #% priceMat = {};
  #% loop and read all files
  
  for(i in 1:length(file_names)){
    tab <- read.csv(paste0(path_to_data, '/', file_names[i]))
    price <- tab[,c(1,2)]
    
    price[,1] <- gsub(",", "", price[,1])
    price[,1] <- as.Date(price[,1], format="%b %d %Y")
    # price[,1]<- as.character(price$Date)
    
    # % if isnumeric(price) == 0
    # %    price = str2double(price);
    # % end 
    
    priceMat[[i]] <- price
    colnames(priceMat[[i]]) <- c("Date, Price")
  }
  
  baseMat <- priceMat[[1]]
  colnames(baseMat)[2] <- substr(file_names[1], 1, 7)
  
  # % baseMat(:,2) = (str2double(baseMat{:,2}));
  
  for(i in 1:(length(priceMat)-1)){
    
    dt <- merge(baseMat, priceMat[[i+1]], by = "Date")
    baseMat <- dt
    colnames(baseMat)[i+2] <- substr(file_names[i+1], 1, 7)
    
  }
  
  # Dates <- as.Date.character(baseMat[,1])
  
  # priceMat = str2double(newMat{:,2:1+length(file_names)});
  return(baseMat)
  
}


normaliseSeries <- function(fxMat, reduce = T){
  # Get the normalised series
  # Normalise by geometric mean
  
  # T and number of exrates
  t <- dim(fxMat)[1]
  
  fxMat <- data.frame(rep(1,t), fxMat)

  # Assign space
  NVals <- fxMat/apply(fxMat, 1, function(x) (prod(x[x!=0]))^(1/sum(x!=0)))
  N_1 <- matrix(NVals[t,], nrow=t, ncol=length(NVals), byrow=TRUE)
  
  # Reduced Normalised Values
  if(reduce){
    NVals <- as.matrix(NVals) / as.numeric(N_1)
  }
  
}


optimWeights <- function(RNVals, init_vals){

  #%UNTITLED3 Find the optimal weights
  #%   Detailed explanation goes here

  #p = inputParser;

  #defaultCovMat = NaN;
  #addRequired(p,'RNVals',@ismatrix);
  #addOptional(p,'CovMat',defaultCovMat);

  # parse(p, RNVals, varargin{:})

  CovMat <- cov(RNVals)
  
  obj <- function(w, CovMat){
    
    w %*% CovMat %*% (w)
    
  }
  
  equal <- function(w, CovMat) {
    sum(w)
  }
  
  n <- nrow(CovMat)
  z <- CovMat
  x <- suppressWarnings(solnp(init_vals, obj, eqfun = equal, eqB = 1, CovMat = z))
  
  return(x$pars)
}



gm_mean = function(x, na.rm=TRUE){
  exp(sum(log(x[x > 0]), na.rm=na.rm) / length(x))
}