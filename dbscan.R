library(proxy)
library(fpc)
library(ggplot2)

rm(list = ls()); gc()
options(stringsAsFactors = F)

df <- iris[,1:3]
df <- data.frame(scale(df, center = T, scale = T))
dist_df <- as.matrix(dist(df, method = "euclidean", diag = T, upper = T))

# dbscan_func
dbscan_func <- function(data, eps, MinPts){
          label <- rep(0, nrow(data))

          for(i in 1:nrow(data)){
                    if(sum(data[i,] <= eps)-1 >= MinPts){
                              label[which(data[i,] <= eps & label == 0)] <- i
                    }
                    for(j in which(data[i,] <= eps & label == i)){
                              if(sum(data[j,] <= eps)-1 >= MinPts){
                                        label[which(data[j,] <= eps & label == 0)] <- i
                              }
                    }
          }
          return(label)
}
label <- dbscan_func(data = dist_df, eps = 1.2, MinPts = 5)
table(label)

# PCA
dm <- as.matrix(df)
dm_cov <- cov(dm) # 计算协方差矩阵
dm_svd <- svd(dm_cov) # 奇异值分解

eigenvec <- dm_svd$u[,1:2] # 特征向量：每个主成分的载荷向量
rownames(eigenvec) <- colnames(dm)
colnames(eigenvec) <- c("PC1","PC2")

dz <- dm%*%eigenvec
dz_1 <- data.frame(dz, label = label)

ggplot(dz_1, aes(x = PC1, y = PC2)) + geom_point(aes(color = factor(label))) 

# dbscan
model2 <- dbscan(df, eps = 0.8, MinPts = 5)
model2$cluster
dz_2 <- data.frame(dz, label = model2$cluster)

ggplot(dz_2, aes(x = PC1, y = PC2)) + geom_point(aes(color = factor(label))) 
