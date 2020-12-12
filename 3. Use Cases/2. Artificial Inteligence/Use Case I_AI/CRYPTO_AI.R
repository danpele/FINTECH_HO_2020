rm(list = ls())

library(PerformanceAnalytics)
library(xts)
library(quantmod)
library(timeSeries)
library(xtable)
library(igraph)
library(tcltk2)
library(MTS)
library(matrixcalc)
library(Matrix)
library(fPortfolio)
library(IntroCompFinR)  #install.packages("IntroCompFinR", repos="http://R-Forge.R-project.org")
require(quadprog)
library(pracma)
library(glasso)

#setwd("/home/rstudio")

prices<-read.table("def.csv", header=TRUE, sep=",", dec=".")
ZOO <- zoo(prices[,-1], order.by=as.Date(as.character(prices$Date), format='%m/%d/%Y'))
head(prices)

#to plot normalized prices

norm_prices<-read.table("def1.csv", header=TRUE, sep=",", dec=".")
ZOO_norm<- zoo(norm_prices[,-1], order.by=as.Date(as.character(norm_prices$Date), format='%m/%d/%Y'))
#dev.new()
plot(ZOO_norm[,c(1,2,4,5,6)], ylab="normalized prices",xlab="time", plot.type= "single", col=c(1:5))
legend("topright", c("BTC","ETH","USDT","BCH","LTC"), col=c(1:5),  cex=0.80, lwd=2)
#dev.new()
plot(ZOO_norm[,c(3,7,8,9,10)], ylab="normalized prices",xlab="time", plot.type= "single", col=c(6:10))
legend("topright", c("XRP","BNB","EOS","XLM","TRX" ), col=c(6:10),  cex=0.80, lwd=2)

return<- Return.calculate(ZOO, method="log")
return<- return[-1, ]
returnstd<-xts(return)
class(return)
dim(return)

summary<-cbind(colMeans(return),colStdevs(return),colKurtosis(return))
colnames(summary)<-c("Mean", "std", "kurtosis")


xtable(round(summary,4))
summary


W<-list()
for(t in 0: 92){
  W[[(t+1)]]=returnstd[(1+t*7):(120+t*7),]
}

W_in<-list()
W_out<-list()


for(t in 1: 93){
  W_in[[(t)]]=W[[t]][c(1:113),]
  W_out[[(t)]]=W[[t]][c(114:120),]
}


C <- list()
lambda_C<-list()
eigen_C<-list()
eigenvec_C<-list()


for(t in 1: length(W_in)){
  #Wt[[(t)]]=returnstd[(t):(tw+t),]
  C[[(t)]] =cor(W_in[[(t)]])
  lambda_C[[t]]<-eigen(C[[t]], symmetric = TRUE)
  eigen_C[[t]]<-lambda_C[[t]]$values
  eigenvec_C[[t]]<-lambda_C[[t]]$vectors
}

M<-matrix(rnorm(10*113, mean=0,sd=1),10,113)
E<-t(M)
O<-M%*%E
L<-1/113
R<-L*O
eigen(R, symmetric = TRUE)
eigen_R<-eigen(R, symmetric = TRUE)$values
Q<-113/10
Q
lambda_max<-(1+1/Q+2*sqrt(1/Q))   ##taking into account the behaviour of the first eigenvalue
lambda_max
lambda_min<-(1+1/Q-2*sqrt(1/Q))
lambda_min

for(i in 1:10){
  for (t in 1:93) {
    if(eigen_C[[t]][i]<lambda_max){eigen_C[[t]][i]=0}
    eigen_C[[t]]<-sort(eigen_C[[t]])
  }
}

filtered_diagonal_C<-list()
V<-list()
f<-list()
C_1<-list()
Dist <- list()

for(t in 1: length(W_in)){
  filtered_diagonal_C[[t]]<-diag(eigen_C[[t]])
  V[[t]]<-eigenvec_C[[t]][,10:1]
  f[[t]]<-t(V[[t]])
  C_1[[t]]<-V[[t]]%*%filtered_diagonal_C[[t]]%*%f[[t]]
  diag(C_1[[t]])<-1
  C_1[[t]]<-as.matrix(C_1[[t]])
  diag(C_1[[t]])<-1
  Dist[[t]]<-sqrt(2-2*C_1[[t]])
  Dist[[t]]<-as.matrix(Dist[[t]])
  Dist[[t]][is.nan(Dist[[t]])]<-0
  colnames(Dist[[(t)]])<-colnames(returnstd)
  rownames(Dist[[(t)]])<-colnames(returnstd)
}


ciao<-list()
#nodes2 <- read.csv("nodes2.csv", header=T, as.is=T)

for(t in 1: length(W)){
  ciao[[t]]<-as.numeric(unlist(Dist[[t]]))
  ciao[[t]]<-matrix(ciao[[t]],10,10)
  colnames(ciao[[t]])<-colnames(returnstd)
  rownames(ciao[[t]])<-colnames(returnstd)
}


A<-list()
network<-list()
Edgelist<-list()
weight<-list()
links2<-list()

for(t in 1: length(W)){
  network[[t]]=graph_from_adjacency_matrix(ciao[[t]],weighted=T, mode="undirected", diag=F)
  Edgelist[[t]]<-get.edgelist(network[[t]])
  weight[[t]]<-E(network[[t]])$weight
  A[[t]]<-cbind(Edgelist[[t]],weight[[t]])
  A[[t]]<-as.matrix(A[[t]])
  links2[[t]]<-as.data.frame(A[[t]])
  colnames(links2[[t]])<-c("from","to","weight")
}


weightmst<-list()
net<-list()
mst<-list()
deg<-list()
root<-list()
deg_vert<-list()
centralization<-list()
def_matrix<-list()
red<-list()
res<-list()
for(t in  1: length(W)){
  net[[t]]<- graph_from_data_frame(d=links2[[t]], directed=F)
  mst[[t]] <- minimum.spanning.tree(net[[t]])
  weightmst[[t]]<-max(E(mst[[t]])$weight)
  wei<-unlist(weightmst[[t]])
  deg[[t]]<-degree(mst[[t]])
  centralization[[t]]<-centr_eigen(mst[[t]])$centralization
  centr<-as.matrix(unlist(centralization))
  deg_vert[[t]]<- -(deg[[t]])
  root[[t]]<-names(deg[[t]])[deg[[t]]== max(deg[[t]])]
  def_matrix[[t]]<-ciao[[t]]-(as_adjacency_matrix(mst[[t]])*ciao[[t]])
  def_matrix[[t]][def_matrix[[t]] == 0] <- 5
  def_m<-as.matrix(unlist(def_matrix[[t]]))
  de<-vec(def_m)
  red[[t]]<-sum(de < wei )/sum(de > wei )
  a<-subset(de,de<wei)
  b<-subset(de,de>wei)
  res[[t]]<-sum(b^-1)/sum(a^-1)
}

## plot MST first window
#dev.new()
verticesdegreeall<-degree(mst[[1]])
node.size<-as.matrix(verticesdegreeall)*2
plot(mst[[1]], edge.color= "black", vertex.size= node.size, vertex.color="orange", layout=layout_with_graphopt)
#id <- tkplot(mst[[1]], activebackground = "white",edge.color= "black", vertex.size= node.size, vertex.color="orange", layout=layout_with_graphopt)
#tkconfigure(igraph:::.tkplot.get(id)$canvas, "bg"="white")

## plot MST last window

#dev.new()
verticesdegreeall<-degree(mst[[93]])
node.size<-as.matrix(verticesdegreeall)*2
plot(mst[[93]], edge.color= "black", vertex.size= node.size, vertex.color="orange", layout=layout_with_graphopt)
#id <- tkplot(mst[[93]], activebackground = "white",edge.color= "black", vertex.size= node.size, vertex.color="orange", layout=layout_with_graphopt)
#tkconfigure(igraph:::.tkplot.get(id)$canvas, "bg"="white")

## plot residuality 

date<-prices$Date
date<-as.Date.factor(date, format="%m/%d/%Y")
#meanlayer1<-vector("logical",140L)
meanlayer1<-list()
for(t in 1: 93){
  meanlayer1[[t]]<-date[[t*7]]
}

meanlayer1<-as.Date(unlist(meanlayer1))
psi<-data.frame(meanlayer1)
fixi<-"2017-09-14"
fixi<-data.frame(fixi)
psi<-t(psi)
gloxi<-data.frame(fixi$fixi,psi)
gloxi
gloxi<-t(gloxi)
gloxi
gloxia<-data.frame(gloxi)
glox<-as.data.frame(gloxia$meanlayer1)
glox<-data.frame(glox)
glox<-glox$gloxia.meanlayer1
glox<-as.matrix(glox)
glox<-glox[-94]
cri<-as.Date(as.character(glox))

weight_mst<-as.matrix(unlist(weightmst))
threshold_mst<-zoo(weight_mst,cri)
residuality<-as.matrix(unlist(res))
residuality<-zoo(residuality,cri)

#dev.new()
plot(threshold_mst, main="",ylab = "", type = "l",xlab="time",ylim=c(1.04,1.55))
par(new = T)
plot(residuality, main="",ylab = "", yaxt="n", type = "l", xlab = "time", col="red")
axis(4,col.axis="red")
#mtext("residuality", side=4, line=3, col="red")
legend("topright", c("max link", "residuality"), col=c("black","red"),  cex=0.65, lwd=2)

EIGEN_cent<-list()
eigencent<-list()

for(t in 1: length(W)){
  EIGEN_cent[[t]]<-eigen_centrality(mst[[t]], directed = FALSE, scale = TRUE,options = arpack_defaults)
  eigencent[[t]]<-EIGEN_cent[[t]]$vector
  round(eigencent[[t]],3)
  eigencent[[t]]<-as.matrix(eigencent[[t]])
  eigencent[[t]]<- -(eigencent[[t]])
}


r<-list()
meanret<-list()
stdev<-list()
g<-list()
COVrmt<-list()

for(t in 1: length(W_in)){
  r[[t]] <- matrix(colMeans(W_in[[t]]), nrow=1)
  meanret[[t]]<-sum(r[[t]])/10
  stdev[[t]]<-apply(W_in[[t]],2,sd)
  stdev[[t]]<-matrix(stdev[[t]]) #sd vector
  rownames(stdev[[t]])<-colnames(W_in[[t]])
  g[[t]]<-stdev[[t]]%*%t(stdev[[t]])
  COVrmt[[t]]<-g[[t]]*C_1[[t]]
  COVrmt[[t]]<-as.matrix(COVrmt[[t]])
}

B<-list()
f<-list()
sol<-list()
w<-list()
z<-list()
z1<-list()
port<-list()
retport<-list()
retport1<-list()
portequally<-list()
retportequally<-list()
retport1equally<-list()


for(t in 1: 93){ 
  B[[t]]<- matrix(1,1,10)
  B[[t]]<- rbind(B[[t]], r[[t]], diag(10),-diag(10))
  f[[t]]<- c(1, meanret[[t]], rep(0,10),rep(-1,10))
  sol[[t]]<- solve.QP(Dmat=COVrmt[[t]], dvec = eigencent[[t]], Amat=t(B[[t]]), bvec=f[[t]], meq=1)
  w[[t]]<-round(sol[[t]]$solution,6)
  w[[t]]<-matrix(w[[t]])
  #z[[t]]<-as.matrix(cbind(nodes2$id,nodes2$class,w[[t]]))
  #z[[t]]<-as.data.frame(z[[t]])
  #colnames(z[[t]])<-c("id","class","weights")
  #z1[[t]]<-z[[t]]$weights
  aus<-repmat(w[[t]],1,7)*t(W_out[[t]])
  aus<-as.matrix(aus)
  port[[t]]<-colSums(aus)
  #VaR_port[[t]]<-VaR(W_out[[t]], weights=w[[t]], p=0.95, portfolio_method ="component", method="modified")$MVaR
  #CVaR_port[[t]]<-CVaR(W_out[[t]], weights=w[[t]], p=0.95, portfolio_method ="component", method="modified")$MES
  retport[[t]]<-mean(colSums(aus))
  retport1[[t]]<-sd(colSums(aus))
  equallyweighted<-matrix(rep(1/10),10,1)
  equallyweighted<-equallyweighted
  ausequally<-repmat(equallyweighted,1,7)*t(W_out[[t]])
  ausequally<-as.matrix(ausequally)
  portequally[[t]]<-colSums(ausequally)
  retportequally[[t]]<-mean(colSums(ausequally))
  retport1equally[[t]]<-sd(colSums(ausequally))
}

pport<-as.matrix(cbind(unlist(port)))
pportequally<-as.matrix(cbind(unlist(portequally)))
p<-cumsum(pport)
pp<-cumsum(pportequally)
retport<-as.matrix(unlist(retport))
retport<-cumsum(retport)
retport1<-as.matrix(unlist(retport1))
retport1<-cumsum(retport1)
retportequally<-as.matrix(unlist(retportequally))
retport1equally<-as.matrix(unlist(retport1equally))


date<-prices$Date
date<-date[-c(1:116)]
date<-as.Date.factor(date, format="%m/%d/%Y")
#meanlayer1<-vector("logical",140L)
meanlayer1<-list()
for(t in 1: 93){
  meanlayer1[[t]]<-date[[t*7]]
}

meanlayer1<-as.Date(unlist(meanlayer1))
psi<-data.frame(meanlayer1)
fixi<-"2018-01-07"
fixi<-data.frame(fixi)
psi<-t(psi)
gloxi<-data.frame(fixi$fixi,psi)
gloxi
gloxi<-t(gloxi)
gloxi
gloxia<-data.frame(gloxi)
glox<-as.data.frame(gloxia$meanlayer1)
glox<-data.frame(glox)
glox<-glox$gloxia.meanlayer1
glox<-as.matrix(glox)
glox<-glox[-94]
cri<-as.Date(as.character(glox))
class(cri)
AI<-matrix(unlist(w),10,93)
rownames(AI)<-colnames(return)
colnames(AI)<-c(1:93)
AI<-t(AI)
xptf<-zoo(AI,cri)
#dev.new()
chart.StackedBar(xptf, ylab= "weight", date.format="%m-%Y", cex.legend = 0.60, colorset=rainbow12equal, main="portfolio temporal composition ??=1")


B<-list()
f<-list()
sol<-list()
w<-list()
z<-list()
z1<-list()
port0.05<-list()


for(t in 1: 93){ 
  B[[t]]<- matrix(1,1,10)
  B[[t]]<- rbind(B[[t]], r[[t]], diag(10),-diag(10))
  f[[t]]<- c(1, meanret[[t]], rep(0,10),rep(-1,10))
  sol[[t]]<- solve.QP(Dmat=COVrmt[[t]], dvec = 0.05*eigencent[[t]], Amat=t(B[[t]]), bvec=f[[t]], meq=1)
  w[[t]]<-round(sol[[t]]$solution,6)
  w[[t]]<-matrix(w[[t]])
  # z[[t]]<-as.matrix(cbind(nodes2$id,nodes2$class,w[[t]]))
  # z[[t]]<-as.data.frame(z[[t]])
  # colnames(z[[t]])<-c("id","class","weights")
  # z1[[t]]<-z[[t]]$weights
  aus<-repmat(w[[t]],1,7)*t(W_out[[t]])
  aus<-as.matrix(aus)
  port0.05[[t]]<-colSums(aus)
}

pport0.05<-as.matrix(cbind(unlist(port0.05)))
p0.05<-cumsum(pport0.05)

B<-list()
f<-list()
sol<-list()
w<-list()
z<-list()
z1<-list()
port0.005<-list()


for(t in 1: 93){ 
  B[[t]]<- matrix(1,1,10)
  B[[t]]<- rbind(B[[t]], r[[t]], diag(10),-diag(10))
  f[[t]]<- c(1, meanret[[t]], rep(0,10),rep(-1,10))
  sol[[t]]<- solve.QP(Dmat=COVrmt[[t]], dvec = 0.005*eigencent[[t]], Amat=t(B[[t]]), bvec=f[[t]], meq=1)
  w[[t]]<-round(sol[[t]]$solution,6)
  w[[t]]<-matrix(w[[t]])
  # z[[t]]<-as.matrix(cbind(nodes2$id,nodes2$class,w[[t]]))
  # z[[t]]<-as.data.frame(z[[t]])
  # colnames(z[[t]])<-c("id","class","weights")
  # z1[[t]]<-z[[t]]$weights
  aus<-repmat(w[[t]],1,7)*t(W_out[[t]])
  aus<-as.matrix(aus)
  port0.005[[t]]<-colSums(aus)
}

pport0.005<-as.matrix(cbind(unlist(port0.005)))
p0.005<-cumsum(pport0.005)

AI<-matrix(unlist(w),10,93)
rownames(AI)<-colnames(return)
colnames(AI)<-c(1:93)
AI<-t(AI)
xptf<-zoo(AI,cri)
chart.StackedBar(xptf, ylab= "weight", date.format="%m-%Y", cex.legend = 0.60, colorset=rainbow12equal, main="portfolio temporal composition ??=0")


B<-list()
f<-list()
sol<-list()
w<-list()
z<-list()
z1<-list()
port0.15<-list()

for(t in 1: 93){ 
  B[[t]]<- matrix(1,1,10)
  B[[t]]<- rbind(B[[t]], r[[t]], diag(10),-diag(10))
  f[[t]]<- c(1, meanret[[t]], rep(0,10),rep(-1,10))
  sol[[t]]<- solve.QP(Dmat=COVrmt[[t]], dvec = 0.15*eigencent[[t]], Amat=t(B[[t]]), bvec=f[[t]], meq=1)
  w[[t]]<-round(sol[[t]]$solution,6)
  w[[t]]<-matrix(w[[t]])
  # z[[t]]<-as.matrix(cbind(nodes2$id,nodes2$class,w[[t]]))
  # z[[t]]<-as.data.frame(z[[t]])
  # colnames(z[[t]])<-c("id","class","weights")
  # z1[[t]]<-z[[t]]$weights
  aus<-repmat(w[[t]],1,7)*t(W_out[[t]])
  aus<-as.matrix(aus)
  port0.15[[t]]<-colSums(aus)
}

pport0.15<-as.matrix(cbind(unlist(port0.15)))
p0.15<-cumsum(pport0.15)

B<-list()
f<-list()
sol<-list()
w<-list()
z<-list()
z1<-list()
port0.025<-list()

for(t in 1: 93){ 
  B[[t]]<- matrix(1,1,10)
  B[[t]]<- rbind(B[[t]], r[[t]], diag(10),-diag(10))
  f[[t]]<- c(1, meanret[[t]], rep(0,10),rep(-1,10))
  sol[[t]]<- solve.QP(Dmat=COVrmt[[t]], dvec = 0.025*eigencent[[t]], Amat=t(B[[t]]), bvec=f[[t]], meq=1)
  w[[t]]<-round(sol[[t]]$solution,6)
  w[[t]]<-matrix(w[[t]])
  #z[[t]]<-as.matrix(cbind(nodes2$id,nodes2$class,w[[t]]))
  #z[[t]]<-as.data.frame(z[[t]])
  #colnames(z[[t]])<-c("id","class","weights")
  #z1[[t]]<-z[[t]]$weights
  aus<-repmat(w[[t]],1,7)*t(W_out[[t]])
  aus<-as.matrix(aus)
  port0.025[[t]]<-colSums(aus)
}

pport0.025<-as.matrix(cbind(unlist(port0.025)))
p0.025<-cumsum(pport0.025)

B<-list()
f<-list()
sol<-list()
w<-list()
z<-list()
z1<-list()
port4<-list()

for(t in 1: 93){ 
  B[[t]]<- matrix(1,1,10)
  B[[t]]<- rbind(B[[t]], r[[t]], diag(10),-diag(10))
  f[[t]]<- c(1, meanret[[t]], rep(0,10),rep(-1,10))
  sol[[t]]<- solve.QP(Dmat=COVrmt[[t]], dvec = 4*eigencent[[t]], Amat=t(B[[t]]), bvec=f[[t]], meq=1)
  w[[t]]<-round(sol[[t]]$solution,6)
  w[[t]]<-matrix(w[[t]])
  # z[[t]]<-as.matrix(cbind(nodes2$id,nodes2$class,w[[t]]))
  # z[[t]]<-as.data.frame(z[[t]])
  # colnames(z[[t]])<-c("id","class","weights")
  # z1[[t]]<-z[[t]]$weights
  aus<-repmat(w[[t]],1,7)*t(W_out[[t]])
  aus<-as.matrix(aus)
  port4[[t]]<-colSums(aus)
}

pport4<-as.matrix(cbind(unlist(port4)))
p4<-cumsum(pport4)


B<-list()
f<-list()
sol<-list()
w<-list()
z<-list()
z1<-list()
port2<-list()

for(t in 1: 93){ 
  B[[t]]<- matrix(1,1,10)
  B[[t]]<- rbind(B[[t]], r[[t]], diag(10),-diag(10))
  f[[t]]<- c(1, meanret[[t]], rep(0,10),rep(-1,10))
  #dvec=rep(0,92) no max  dei rendimenti ma solo vincolo
  sol[[t]]<- solve.QP(Dmat=COVrmt[[t]], dvec = 2*eigencent[[t]], Amat=t(B[[t]]), bvec=f[[t]], meq=1)
  w[[t]]<-round(sol[[t]]$solution,6)
  w[[t]]<-matrix(w[[t]])
  # z[[t]]<-as.matrix(cbind(nodes2$id,nodes2$class,w[[t]]))
  # z[[t]]<-as.data.frame(z[[t]])
  # colnames(z[[t]])<-c("id","class","weights")
  # z1[[t]]<-z[[t]]$weights
  aus<-repmat(w[[t]],1,7)*t(W_out[[t]])
  aus<-as.matrix(aus)
  port2[[t]]<-colSums(aus)
}

pport2<-as.matrix(cbind(unlist(port2)))
pp2<-cumsum(pport2)


B<-list()
f<-list()
sol<-list()
w<-list()
z<-list()
z1<-list()
port0.7<-list()

for(t in 1: 93){ 
  B[[t]]<- matrix(1,1,10)
  B[[t]]<- rbind(B[[t]], r[[t]], diag(10),-diag(10))
  f[[t]]<- c(1, meanret[[t]], rep(0,10),rep(-1,10))
  #dvec=rep(0,92) no max  dei rendimenti ma solo vincolo
  sol[[t]]<- solve.QP(Dmat=COVrmt[[t]], dvec = 0.7*eigencent[[t]], Amat=t(B[[t]]), bvec=f[[t]], meq=1)
  w[[t]]<-round(sol[[t]]$solution,6)
  w[[t]]<-matrix(w[[t]])
  # z[[t]]<-as.matrix(cbind(nodes2$id,nodes2$class,w[[t]]))
  # z[[t]]<-as.data.frame(z[[t]])
  # colnames(z[[t]])<-c("id","class","weights")
  # z1[[t]]<-z[[t]]$weights
  aus<-repmat(w[[t]],1,7)*t(W_out[[t]])
  aus<-as.matrix(aus)
  port0.7[[t]]<-colSums(aus)
}

pport0.7<-as.matrix(cbind(unlist(port0.7)))
p0.7<-cumsum(pport0.7)

B<-list()
f<-list()
sol<-list()
w<-list()
z<-list()
z1<-list()
port0<-list()

for(t in 1: 93){ 
  B[[t]]<- matrix(1,1,10)
  B[[t]]<- rbind(B[[t]], r[[t]], diag(10),-diag(10))
  f[[t]]<- c(1, meanret[[t]], rep(0,10),rep(-1,10))
  #dvec=rep(0,92) no max  dei rendimenti ma solo vincolo
  sol[[t]]<- solve.QP(Dmat=COVrmt[[t]], dvec = matrix(0,10,1), Amat=t(B[[t]]), bvec=f[[t]], meq=1)
  w[[t]]<-round(sol[[t]]$solution,6)
  w[[t]]<-matrix(w[[t]])
  # z[[t]]<-as.matrix(cbind(nodes2$id,nodes2$class,w[[t]]))
  # z[[t]]<-as.data.frame(z[[t]])
  # colnames(z[[t]])<-c("id","class","weights")
  # z1[[t]]<-z[[t]]$weights
  aus<-repmat(w[[t]],1,7)*t(W_out[[t]])
  aus<-as.matrix(aus)
  port0[[t]]<-colSums(aus)
}

pport0<-as.matrix(cbind(unlist(port0)))
p0<-cumsum(pport0)

## BENCHMARK

MSW_prezzi<-read.table("crix_data.csv", header=TRUE, sep=",", dec=".")
ZOOMSW<- zoo(MSW_prezzi[,-1], order.by=as.Date(as.character(MSW_prezzi$Date), format='%m/%d/%Y'))
MSW<- Return.calculate(ZOOMSW, method="log")
MSW<- MSW[-1, ]
MSW<-xts(MSW)
View(MSW)
dim(MSW)
MSW<-MSW[-c(765:770),]
#tail(MSW)
MSW1<-MSW[-c(1:113)]
MSW_cum<-cumsum(MSW1)

W_MSW<-list()

for(t in 0: 92){
  W_MSW[[(t+1)]]=MSW[(1+t*7):(t*7+120),]
}

W_MSW_in<-list()
W_MSW_out<-list()


for(t in 1: 93){
  W_MSW_in[[(t)]]=W_MSW[[t]][c(1:113),]
  W_MSW_out[[(t)]]=W_MSW[[t]][c(114:120),]
}

# VaR_MSW<-list()
# CVaR_MSW<-list()
# 
# for(t in 1: 93){
#   VaR_MSW[[t]]<-VaR(W_MSW_out[[t]], p=0.95, portfolio_method ="single", method="modified")
#   CVaR_MSW[[t]]<-CVaR(W_MSW_out[[t]], p=0.95, portfolio_method ="single", method="modified")
#   
# }

## GLASSO

C<- list()
C_glasso<-list()

for(t in 1: length(W_in)){
  print(t)
  C[[(t)]] =var(W_in[[(t)]])
  C_glasso[[(t)]]<-glasso(C[[(t)]], rho=0.01)$w
  C_glasso[[(t)]]<-round(C_glasso[[(t)]],4)
}


B<-list()
f<-list()
sol<-list()
w<-list()
z<-list()
z1<-list()
port_glasso<-list()

for(t in 1: 93){ 
  print(t)
  B[[t]]<- matrix(1,1,10)
  B[[t]]<- rbind(B[[t]], r[[t]], diag(10),-diag(10))
  f[[t]]<- c(1, meanret[[t]], rep(0,10),rep(-1,10))
  sol[[t]]<- solve.QP(Dmat=C_glasso[[t]], dvec = matrix(0,10,1), Amat=t(B[[t]]), bvec=f[[t]], meq=1)
  w[[t]]<-round(sol[[t]]$solution,6)
  w[[t]]<-matrix(w[[t]])
  aus<-repmat(w[[t]],1,7)*t(W_out[[t]])
  aus<-as.matrix(aus)
  port_glasso[[t]]<-colSums(aus)
}

pport_glasso<-as.matrix(cbind(unlist(port_glasso)))
p_glasso_cum<-cumsum(pport_glasso)


RET<-cbind(pport_glasso,pportequally, pport0, pport0.005,pport0.025, pport0.05,pport0.15,pport0.7,pport)
colnames(RET)<-c("glasso", "equally weighted", "??=0", "??=0.005","??=0.025","??=0.05", "??=0.15","??=0.7","??=1")
RET_cum<-cbind(p_glasso_cum,pp, p0, p0.005, p0.025, p0.05, p0.15, p0.7, p) 
RET_cum<-RET_cum-0.001*92
RET_cum<-cbind(MSW_cum,RET_cum)
RET_cum<-zoo(RET_cum)
colnames(RET_cum)<-c("Benchmark (CRIX)","Glasso Markowitz", "Equally Weighted", "Network Markowitz", "??=0.005","??=0.025","??=0.05", "??=0.15","??=0.7","??=1")
# dev.new()
# plot(RET_cum, screens=1, col=c("black", "lightblue", "pink","blue","red","brown", "gold", "grey", "green", "violet"), lwd=2, ylab="Cumulative P&L", xlab="time")
# legend("bottomleft", c("Benchmark (CRIX)","Glasso Markowitz", "Equally Weighted", "Network Markowitz", "??=0.005","??=0.025","??=0.05", "??=0.15","??=0.7","??=1"), col=c("black", "lightblue", "pink","blue","red","brown", "gold", "grey", "green", "violet"),  cex=0.65, lwd=3.5)

# library(xlsx)
# write.xlsx(RET_cum, file="profit_crypto.xlsx", sheetName="sheet1", row.names=FALSE)


## EMPIRICAL COV MATRIX (one etf per class)


W<-list()
for(t in 0: 92){
  W[[(t+1)]]=returnstd[(1+t*7):(120+t*7),]
}

W_in<-list()
W_out<-list()


for(t in 1: 93){
  W_in[[(t)]]=W[[t]][c(1:113),]
  W_out[[(t)]]=W[[t]][c(114:120),]
}


C <- list()

for(t in 1: length(W_in)){
  C[[(t)]] =cor(W_in[[(t)]])
}


Dist <- list()

for(t in 1: length(W_in)){
  C[[t]]<-as.matrix(C[[t]])
  Dist[[t]]<-sqrt(2-2*C[[t]])
  Dist[[t]]<-as.matrix(Dist[[t]])
  Dist[[t]][is.nan(Dist[[t]])]<-0
  colnames(Dist[[(t)]])<-colnames(returnstd)
  rownames(Dist[[(t)]])<-colnames(returnstd)
}


ciao<-list()

for(t in 1: length(W)){
  ciao[[t]]<-as.numeric(unlist(Dist[[t]]))
  ciao[[t]]<-matrix(ciao[[t]],10,10)
  colnames(ciao[[t]])<-colnames(returnstd)
  rownames(ciao[[t]])<-colnames(returnstd)
}


A<-list()
network<-list()
Edgelist<-list()
weight<-list()
links2<-list()

for(t in 1: length(W)){
  network[[t]]=graph_from_adjacency_matrix(ciao[[t]],weighted=T, mode="undirected", diag=F)
  Edgelist[[t]]<-get.edgelist(network[[t]])
  weight[[t]]<-E(network[[t]])$weight
  A[[t]]<-cbind(Edgelist[[t]],weight[[t]])
  A[[t]]<-as.matrix(A[[t]])
  links2[[t]]<-as.data.frame(A[[t]])
  colnames(links2[[t]])<-c("from","to","weight")
}

net<-list()
mst<-list()

for(t in  1: length(W)){
  net[[t]]<- graph_from_data_frame(d=links2[[t]], directed=F)
  mst[[t]] <- minimum.spanning.tree(net[[t]])
}

EIGEN_cent<-list()
eigencent<-list()
bet<-list()

for(t in 1: length(W)){
  
  EIGEN_cent[[t]]<-eigen_centrality(mst[[t]], directed = FALSE, scale = TRUE,options = arpack_defaults)
  eigencent[[t]]<-EIGEN_cent[[t]]$vector
  round(eigencent[[t]],3)
  eigencent[[t]]<-as.matrix(eigencent[[t]])
  bet[[t]]<-as.matrix(betweenness(mst[[t]], directed = F))
  eigencent[[t]]<- -(eigencent[[t]])
  bet[[t]]<- -(bet[[t]])
}

r<-list()
meanret<-list()
stdev<-list()
g<-list()
COV<-list()


for(t in 1: length(W_in)){
  r[[t]] <- matrix(colMeans(W_in[[t]]), nrow=1)
  meanret[[t]]<-sum(r[[t]])/92
  stdev[[t]]<-apply(W_in[[t]],2,sd)
  stdev[[t]]<-matrix(stdev[[t]]) #sd vector
  rownames(stdev[[t]])<-colnames(W_in[[t]])
  g[[t]]<-stdev[[t]]%*%t(stdev[[t]])
  COV[[t]]<-g[[t]]*C[[t]]
  COV[[t]]<-as.matrix(COV[[t]])
}


B<-list()
f<-list()
sol<-list()
w<-list()
z<-list()
z1<-list()
port_emp_cov<-list()


for(t in 1: 93){ 
  B[[t]]<- matrix(1,1,10)
  B[[t]]<- rbind(B[[t]], r[[t]], diag(10),-diag(10))
  f[[t]]<- c(1, meanret[[t]], rep(0,10),rep(-1,10))
  sol[[t]]<- solve.QP(Dmat=COV[[t]], dvec = 0.005*eigencent[[t]], Amat=t(B[[t]]), bvec=f[[t]], meq=1)
  w[[t]]<-round(sol[[t]]$solution,6)
  w[[t]]<-matrix(w[[t]])
  # z[[t]]<-as.matrix(cbind(nodes$id,nodes$class,w[[t]]))
  # z[[t]]<-as.data.frame(z[[t]])
  # colnames(z[[t]])<-c("id","class","weights")
  # z1[[t]]<-z[[t]]$weights
  aus<-repmat(w[[t]],1,7)*t(W_out[[t]])
  aus<-as.matrix(aus)
  port_emp_cov[[t]]<-colSums(aus)
}

port_emp_cov<-as.matrix(cbind(unlist(port_emp_cov)))
ret_cum_empcov<-cumsum(port_emp_cov)

# cumulative returns

defi<-cbind(RET_cum,ret_cum_empcov)
colnames(defi)<-c("Benchmark (CRIX)","Glasso Markowitz", "Equally Weighted", "Network Markowitz", "??=0.005","??=0.025","??=0.05", "??=0.15","??=0.7","??=1","Classical Markowitz")

#Plot returns

prezzi<-read.table("rend100_crypto.csv", header=TRUE, sep=",", dec=".")
ZOO <- zoo(prezzi[,-1], order.by=as.Date(as.character(prezzi$Date), format='%m/%d/%Y'))
head(ZOO)

#dev.new()
plot(ZOO, screens=1, col=c("black", "lightblue", "pink","darkslateblue","red","brown", "gold", "grey", "green", "blue3","darkorchid2"), ylab="P&L", xlab="time")
legend("topright", c("Benchmark (CRIX)","Glasso Markowitz", "Equally Weighted", "Network Markowitz", "??=0.005","??=0.025","??=0.05", "??=0.15","??=0.7","??=1","Classical Markowitz"), col=c("black", "lightblue", "pink","darkslateblue","red","brown", "gold", "grey", "green", "blue3","darkorchid2"),  cex=0.5, lwd=2)

# colnames(ZOO)

#dev.new()
plot(ZOO[,c(1,2,3,4,11)], screens=1, col=c("black", "lightblue", "pink","darkslateblue", "darkorchid2"), lwd=1, ylab="P&L", xlab="time")
legend("topright", c("Benchmark (CRIX)","Glasso Markowitz", "Equally Weighted", "Network Markowitz", "Classical Markowitz"), col=c("black", "lightblue", "pink","darkslateblue","darkorchid2"),  cex=0.5, lwd=2)
