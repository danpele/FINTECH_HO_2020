# # Name of Quantlet: VCS_cryptos
#w1: the data set
#M: the number of angles used for projection directions, 
#as in in equation (11) of Yatrakos (2013)
#componentsizes: the number of data points in each component

#We have K components in the data (for example, the data correspond to
#assets and we have K=4 components, in particular cryptos, stocks, exchange, commodities)
#This program uses the MVCS clustering method of Yaracos (2013). In
#particular, this code is used is to find how many of the projection directions 
#used will give perfect classification for each component, in one run.
#As projection directions, we use the discretized projection directions of
#equation (11) of Yatrakos (2013)
#This code was created bu Michalis Kolossiatis and Yannis Yatracos.

#################################################################
#- Clear Environment -#
rm(list = ls())
graphics.off()

#setwd("D:\\PROIECTE\\HORIZON 2020\\Use Case DP\\VCS_Cryptos")
setwd("/home/rstudio/VCS_cryptos/")

#Packages

# install.packages("matlab")


#- Libraries -#

library(matlab)
#- Source R functions to be used -#

source('weigthsall.R')

#################################################
#- Data to read -#
#################################################

M=2
componentsizes=c(150,496,13,20)

data <- read.csv("23D.csv") #static dataset
w1=data[,4:9]
n=nrow(w1) #n is the data size, d is the data dimension
d=ncol(w1)
K=length(componentsizes) #the number of components
niter=M^(d-1) #the number of projection directions examined
maxmaxw=zeros(K,1) #the maximum index value, among those that  give perfect classification.
minpropB=ones(K,1) #the minimum misclassification proportions
bestproj=zeros(K,d)  #the projection direction that corresponds to the minimum misclassification proportion
kkk=ones(K,1) #the number of projection directions that give perfect classification
truegroups=cell(K,1)

truegroups[[1]]=1:componentsizes[1]
ind=componentsizes[1]
for (jj in (2:K))
{
truegroups[[jj]]=(ind+1):(ind+componentsizes[jj])
ind=ind+componentsizes[jj]
}

matrixminpropB=zeros(K,d+1)

a=0.95
xa=-log(-log(a))
signif=(log(n)+xa)/n
signifindex=zeros(K,1) #the number of maximum index values 
#that are statistically not significant, among those that give perfect classification.

flip=t(zeros(d-1,1))
k=t(zeros(d-1,1))

for (i in (1:niter))
{
ucoeff=ones(1,d) #the discretized projection directions
k[d-1]=k[d-1]+1
  for (j in seq(from=(d-2), to=-1, by=-1))
  {
  flip[j+1]=(mod(k[j+1],M)-1==0)
  flipsum=sum(flip[(j+1):(d-1)])
  chk=t(ones(d-1-j,1))
          chksum=sum(chk)
          if (flipsum==chksum)
          {
              k[j]=k[j]+1
              k[j+1]=1
          }
    }
    
    for (jjj in (1:(d-1)))
    {
        ucoeff[1]=ucoeff[1]*cos(pi*k[jjj]/M)
    }
    ucoeff[d]=ucoeff[d]*sin(pi*k[d-1]/M)
    if (d>2)
    {
        for (n1 in (2:(d-1)))
        {
            ucoeff[n1]=ucoeff[n1]*sin(pi*k[n1-1]/M)
            for (n2 in n1:(d-1))
            {
                ucoeff[n1]=ucoeff[n1]*cos(pi*k[n2]/M)
            }
        }
    }


w2=(as.matrix(w1))%*%t(as.matrix(ucoeff))#the projected data
wall=weightsall(w2,truegroups)#cluster1,cluster2 are the clusters generated
cluster1=wall$cluster1
cluster2=wall$cluster2
indicat1=wall$indicat1
indicat2=wall$indicat2
maxw=wall$maxw
#idicat1, indicat2 are the indicators to which cluster each data point is allocated
#maxw is the maximum index value


for (jj in (1:K))
{
propcl1=1-length(indicat1[[jj]])/componentsizes[jj]
propcl2=1-length(indicat2[[jj]])/componentsizes[jj]
prop=min(propcl1,propcl2)
propcl1A=1-length(indicat1[[jj]])/length(cluster1)
propcl2A=1-length(indicat2[[jj]])/length(cluster2)
propA=min(propcl1A,propcl2A) 
propB=max(propA,prop)


if (propB<minpropB[jj])
{
minpropB[jj]=propB
bestproj[jj,]=ucoeff #We store the projection
#direction that corresponds to the best classification 
#(in terms of smallest misclassification proportion)
}

if (propB==0)
{
  kkk[jj]=kkk[jj]+1

if (maxw>maxmaxw[jj])
  {
    matrixminpropB[jj,]=cbind(maxw,ucoeff) #We store the projection direction that
    #corresponds to the maximum index value, among those
    #that  give perfect classification. We also keep that maximum index value
    maxmaxw[jj]=maxw
  }
if (maxw<signif)
{
signifindex[jj,1]=signifindex[jj,1]+1
}
}
}
}
kkk=kkk-1



#==========================================================================
  


#Output

a=0.95
xa=-log(-log(a))
signif=(log(n)+xa)/n
#Output
print('=========================================================================')
print('Results:')
fprintf('\n')
print(paste('Data dimension: ',d))
print(paste('Number of data components: ',K))
print(paste('Data components sizes: ',t(componentsizes)))
print(paste('Number of angles used for projection directions: ' ,M))

angles=1:M
angles=(180/M*angles)
print(paste('The angles (in degrees) used in projection directions were 
       from the set: ',angles))
print(paste('Critical value for  significance of the index value is ', signif))
fprintf('\n')
print(paste('Number of projection directions examined: ' ,niter))
fprintf('\n')
print('Component  Minimum misclassification proportion  
      Number of projection directions that give perfect classification')
for (jj in (1:K))
{
    print(c(jj ,minpropB[jj],kkk[jj]))
}
fprintf('\n')
fprintf('\n')

print('Results for each component:')
for (jj in (1:K))
{
 fprintf('\n')
 if (kkk[jj]==0)
  {   bestproj=as.matrix(bestproj)
      tb=t(bestproj)
      w1=as.matrix(w1)
      w2=w1%*%tb[,jj]
      wall=weightsall(w2,truegroups)#cluster1,cluster2 are the clusters generated
      cluster1=wall$cluster1
      cluster2=wall$cluster2
      indicat1=wall$indicat1
      indicat2=wall$indicat2
      maxw=wall$maxw
      propcl1=1-length(indicat1[[jj]])/componentsizes[jj]
      propcl2=1-length(indicat2[[jj]])/componentsizes[jj]
      prop=min(propcl1,propcl2)
      propcl1A=1-length(indicat1[[jj]])/length(cluster1)
      propcl2A=1-length(indicat2[[jj]])/length(cluster2)
      propA=min(propcl1A,propcl2A) 

      #propB=max(propA,prop)
      set1=truegroups[jj,1]
      set0=1:n
      set2=setdiff(set0,set1)
        if (propA>prop) #this means that propB=propA
        {
        if (propcl1A>propcl2A) #this means that propA=propcl2A and the selected cluster is cluster2
         { extraobs=intersect(cluster2,set2)  #those from outside the component that are in the selected cluster
          leftoutobs=intersect(cluster1,set1) #those from the component that are in the other cluster
         }
        else #this means that propA=propcl1A and the selected cluster is cluster1
        {
        extraobs=intersect(cluster1,set2)  #those from outside the component that are in the selected cluster
        leftoutobs=intersect(cluster2,set1) #those from the component that are in the other cluster
         }
      } 

      else #this means that propB=prop
      {
        if (propcl1>propcl2) #this means that prop=propcl2 and the selected cluster is cluster2
        {  extraobs=intersect(cluster2,set2)  #those from outside the component that are in the selected cluster
          leftoutobs=intersect(cluster1,set1) #those from the component that are in the other cluster
        }
      else #this means that prop=propcl1 and the selected cluster is cluster1
        {
          extraobs=intersect(cluster1,set2)  #those from outside the component that are in the selected cluster
        leftoutobs=intersect(cluster2,set1) #those from the component that are in the other cluster
        }
        
      }
      print(paste('No perfect classification for component ', jj))  # 
      print('The best classification of the elements of this component, corresponding to the minimum misclassification proportion above, is as follows:')
      print(paste(length(leftoutobs), ' observations from component ', jj,
        ' are misclassified, i.e clustered with the observations from all other categories.'))
      print(paste(length(extraobs),' observations from other categories are 
                   misclassified, i.e. clustered together with the observations of component',
                jj))
    }

    else
    {
        print(paste('The maximum index value that provided perfect classification 
                    for component ',jj,' was ' ,maxmaxw[jj]))
      print('and the corresponding projection direction was')
      print(matrixminpropB[jj,2:length(matrixminpropB)])
      
      if (signifindex[jj]==0)
      {
      print('All projection directions that provided perfect 
             classification had index values that were statistically significant.')
       }   
      else
          {
            print(paste('The number of projection directions that
                  provided perfect classification, and whose 
                  index value was statistically significant,
                  was ' ,kkk[jj]-signifindex[jj]))
           }
      }
    }

    
