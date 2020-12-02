
 # kmeans_opt(X) returns the output of the k-means
 # algorithm with the optimal number of clusters, as determined by the ELBOW
 # method.

kmeans_opt <- function(X) 
  {
calc_vec2mat_dist = function(x, ref_mat) {
  # compute row-wise vec2vec distance 
  apply(ref_mat, 1, function(r) sum((r - x)^2))
}
calc_mat2mat_dist = function(input_mat, ref_mat) 
  {
  
  dist_mat = apply(input_mat, 1, function(r) calc_vec2mat_dist(r, ref_mat))
  
  # transpose to have each row for each input datapoint
  # each column for each centroids
  cbind(t(dist_mat), max.col(-t(dist_mat)))
}

set.seed(1)
X=F


m=nrow(X) #getting the number of samples
ToTest=ceil(sqrt(m))
Cutoff=0.90
Repeats=30
#unit-normalize
MIN=min(X)
MAX=max(X)
X=(X-MIN)/(MAX-MIN)


D=zeros(ToTest,1) #initialize the results matrix
for (c in (1:ToTest))
{#for each sample

dist=kmeans(X,c)$tot.withinss #compute the sum of intra-cluster distances
tmp=sum(dist) #best so far


for (cc in (2:Repeats)) #repeat the algo
{
  dist=kmeans(X,c)$tot.withinss
tmp=min(sum(dist),tmp);

D[c,1]=tmp; #collect the best so far in the results vecor
}
}


Var=D[1:(nrow(D)-1)]-D[2:nrow(D)]; #calculate variance explained
PC=cumsum(Var)/(D[1]-D[nrow(D)]);

K=min(which(PC >= Cutoff)) #find the best index
K
km=kmeans(X,K) #now rerun one last time with the optimal number of clusters
C=km$centers
C=C*(MAX-MIN)+MIN
IDX=calc_mat2mat_dist(X, km$centers)[,12]
return(list(C=C, K=K, IDX=IDX))

}


