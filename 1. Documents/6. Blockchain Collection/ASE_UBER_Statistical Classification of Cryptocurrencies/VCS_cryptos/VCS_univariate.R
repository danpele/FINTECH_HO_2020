# Description: 'This quantlet applies the Variance Components Split in the univariate case.
# Keywords: cryptocurrency, Variance Components Split, 
# classiffication, multivariate analysis, complete separation
# Authors: Michalis Kolossiatis and Yannis Yatracos
# Paper: Yatracos, Y. G. (2013) Detecting clusters in the data from variance decompositions 
# of its projections.J. Classificn,30, 30-55.

#################################################################
#- Clear Environment -#
rm(list = ls())
graphics.off()

setwd("D:\\PROIECTE\\HORIZON 2020\\Use Case DP\\VCS_Cryptos")


#################################################
#- Data to read -#
#################################################

M=2;
componentsizes=c(150,496,13,20)

data <- read.csv("23D.csv") #static dataset
weights1<-function (x1)
{
  { 
    #x1<-x1$data
    X1<-x1
    x <- x1[order(x1)]
    oo <- (1:length(x1))[order(x1)]
    D <- length(x) * (length(x) - 1) * var(x)
    m <- 1:length(x)
    l <- (length(x) - 1):1
    xd <- diff(x)  #      oxd <- 1:length(xd)
    w <- rep(0, length(xd))
    prw <- rep(0, length(xd))
    ss <- rep(0, length(xd))
    for(i in 1:(length(xd) - 1)) {
      r1h <- xd[1:i] * m[1:i]
      r1 <- sum(r1h)
      r2h <- xd[(i + 1):length(xd)] * l[(i + 1):length(xd)]
      r2 <- sum(r2h)
      ss[i] <- (length(x) - i) * r1 + i * r2
      w[i] <- (xd[i] * ss[i])/D
    }
    ss[length(xd)] <- sum(xd[1:length(xd)] * m[1:length(xd)])
    w[length(xd)] <- (xd[length(xd)] * ss[length(xd)])/D
    ###
    #      print(xd)      #      print(D)
    print("TABLE OF ORIGINAL (X,Y) ORDER AND NEW ORDER WITH X INCREASING")
    print(cbind(x, (1:length(x)), oo))
    print("TABLE OF WEIGHTS  IN NEW ORDER")
    print(cbind((1:length(xd)), w))
    print("SUM OF WEIGHTS IS")
    print(sum(w))
    we <- cbind(1:length(xd), w)
    #      print(we)      #those below are added
    #      print(max(w))  #      print(order(w))
    #      print(w[order(w)])
    print("TOTAL NUMBER OF CASES")
    print(length(x1))
    megw <- (1:length(w))[w == max(w)]
    print("MAXIMUM WEIGHT=")
    print(max(w))  #      print("CORRESPONDS TO WEIGHT=")
    print("NUMBER OF OBSERVATIONS TO THE LEFT OF THE SELECTED GAP")
    print(megw)    #      print("CORRESPONDING SELECTED OBSERVATIONS")
    print("CORRESPONDING SELECTED ORIGINAL  OBSERVATIONS")
    print(c(oo[megw], oo[megw + 1]))
    print("DIVIDING OBSERVATIONS")
    print(c(X1[oo[megw]], X1[oo[megw+1]]))
    print("AVERAGE OF DIVIDING OBSERVATIONS")
    print((X1[oo[megw]]+ X1[oo[megw +1]])/2)
  }
  MesosOros<-(X1[oo[megw]]+ X1[oo[megw +1]])/2
}

weights1(data[,4])
