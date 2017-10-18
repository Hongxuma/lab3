library(ggplot2)
library(dplyr)
library(tidyverse)
library(ggpubr)
library(foreach)
library(doParallel)

load("data/lingBinary.RData")
Data <- lingBinary[,c(7:474)]

Data.test <- Data[c(1:5000),]
#Data.kmeans <- kmeans(Data.test,5)

nCores <- 4  # to set manually 
cl <- makeCluster(nCores) 

# matrix for similiarity calculation
labelingMatrix <- function(Kmeans){
  
  Cluster <- unname(Kmeans[1][[1]])
  size <- length(Cluster)
  C <- matrix(0, nrow = size, ncol = size)
  
  for (i in 1:size){
    for (j in 1:size){
      if (Cluster[i] == Cluster[j]){
        C[i,j] = 1
      }
      
    }
  }
  return(C)
}

labelingMatrixCommon <- function(Kmeans,common){
  Cluster <- unname(Kmeans[1][[1]][common])
  size <- length(common)
  C <- matrix(0, nrow = size, ncol = size)
  
  for (i in 1:size){
    for (j in 1:size){
      if (Cluster[i] == Cluster[j]){
        C[i,j] = 1
      }
      
    }
  }
  return(C)
}

Jaccard <- function(C1,C2){
  N11 <- table(C1[C2 == 1])[2][[1]]
  N01 <- table(C1[C2 == 0])[2][[1]]
  N10 <- table(C1[C2 == 1])[1][[1]]
  return(N11/(N01+N10+N11))
}

ClusteringSimilarity <- function(X, kmax, N, m){
  S <- matrix(0, nrow = N, ncol = kmax)
  for (k in 2:kmax){
    for (i in 1:N){
      sub1 <- X[sample(nrow(X),round(nrow(X)*m)),]
      sub2 <- X[sample(nrow(X),round(nrow(X)*m)),]
      sub1.kmeans <- kmeans(sub1,5)
      sub2.kmeans <- kmeans(sub2,5)
      common <- intersect(rownames(sub1),rownames(sub2))
      #sub1.intersect <- sub1[common,]
      #sub2.intersect <- sub2[common,]
      
      sub1.labelingMatrix <- labelingMatrixCommon(sub1.kmeans,common)
      sub2.labelingMatrix <- labelingMatrixCommon(sub2.kmeans,common)
      S[i,k] = Jaccard(sub1.labelingMatrix,sub2.labelingMatrix)
    }
  }
  return(S)
  
}

ClusteringSimilarityPall <- function(k, X, N, m){
  labelingMatrixCommon <- function(Kmeans,common){
    Cluster <- unname(Kmeans[1][[1]][common])
    size <- length(common)
    C <- matrix(0, nrow = size, ncol = size)
    
    for (i in 1:size){
      for (j in 1:size){
        if (Cluster[i] == Cluster[j]){
          C[i,j] = 1
        }
        
      }
    }
    return(C)
  }
  Jaccard <- function(C1,C2){
    N11 <- table(C1[C2 == 1])[2][[1]]
    N01 <- table(C1[C2 == 0])[2][[1]]
    N10 <- table(C1[C2 == 1])[1][[1]]
    return(N11/(N01+N10+N11))
  }
  S <- matrix(0, nrow = N+1, ncol = 1)
  S[1,1] <- k
    for (i in 1:N){
      sub1 <- X[sample(nrow(X),round(nrow(X)*m)),]
      sub2 <- X[sample(nrow(X),round(nrow(X)*m)),]
      sub1.kmeans <- kmeans(sub1,5)
      sub2.kmeans <- kmeans(sub2,5)
      common <- intersect(rownames(sub1),rownames(sub2))
      #sub1.intersect <- sub1[common,]
      #sub2.intersect <- sub2[common,]
      
      sub1.labelingMatrix <- labelingMatrixCommon(sub1.kmeans,common)
      sub2.labelingMatrix <- labelingMatrixCommon(sub2.kmeans,common)
      S[i+1,1] = Jaccard(sub1.labelingMatrix,sub2.labelingMatrix)
    }
  return(S)
  
}

result <- parLapply(cl = cl, 
                    X = 2:10,
                    FUN = ClusteringSimilarityPall,
                    # extra arguments to looFit
                    Data.test, 100, 0.8)

#Result <- ClusteringSimilarity(Data.test,10,100,0.8)

result_df <- data.frame(results = unlist(result))

# remember to save the results of your analysis if 
# you're running it using a shell script!
write.csv(result_df, "results_foreach.csv")