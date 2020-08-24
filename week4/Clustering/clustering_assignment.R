# Clustering Analysis
# 1. Selection of attributes that are considered important for clustering data
# 2. Data Normalization -> Euclidean Distance
# 3. Outlier Screening
# 4. Selection of Clustering Algorithm - K-means
# 5. Decision of the Number of Clusters
# K-means
# 1. Randomly set k cluster center points among objects in the data
# 2. All objects are assigned to the cluster to which the centeroid of the nearest cluster belongs
# 3. Reset the centeroid of each cluster
# 4. Repeat 1 to 4 times until the cluster center point does not change
# algorithm stops if cluster does not change after iteration
# the primary features of K-means : 1) initial setup for a centroid of the first cluster 2) the number of clusters
# initial setup for the first centroid
# random split : the amount of data of the centeroid
# forgy algorithm : random selection of k objects
# the number of clusters
# heuristic algorithms
# elbow point

# Set Up
rm(list=ls())

# Data Load
data <- iris[,1:4]

# K-means Clustering Algorithm Implementation
k_means<-function(data,k,n) # data, k for the number of clusters, n for the number of variables
{
  # make centeroids with forgy algortihm forgy
  centr_idx<-sample(nrow(data),k)
  centr<-data[centr_idx,]
  
  # k_means algorithm
  while(1){# infinite loop until there is no difference between the previous centeroid and the previous centeroid
    
    # convert it to a matrix to facilitate distance calculation
    t<-rbind(centr,data) # combine
    d<-as.matrix(dist(t)) # calculate distance
    d<-d[,1:k] # only the distance to the centeroid needed, so remove the rest
    dmin_idx<-apply(d,1,which.min) # extracting the index at which each distance is the minimum
    res<-cbind(t,dmin_idx) # add dmin_idx col
    res<-res[(1+k):(nrow(data)+k),] # the rest
    
    # mean calculation for the best proper new centeroid
    new_centr<-data.frame(c(1:k)) # create an empty dataframe
    for (i in 1:n){
      tmp<-aggregate(x=res[,i],by=list(dmin_idx=res$dmin_idx),mean)
      new_centr<-cbind(new_centr,tmp[,2])
    }
    new_centr<-new_centr[c(1:n+1)] # extract only the mean value
    
    # condition
    if(centr-new_centr<1e-5){ # if there is little difference from the previous center point (if we put the condition like 'if they are the same', there is a possibility of an infinite loop)
      return(list(res$dmin_idx)) # return
    }
    else{
      centr=new_centr # else, update the new centeroid
    }
  }
}
k_means(data,5,4)