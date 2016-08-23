rm(list = ls())
gc()

library(ggplot2)

df <- USArrests

dm <- as.matrix(df)
dm <- scale(dm, center = T, scale = T) # 数据标准化

dm_cov <- cov(dm) # 计算协方差矩阵
dm_svd <- svd(dm_cov) # 奇异值分解

dm_svd$d # 特征值
dm_svd$d/sum(dm_svd$d) # 每个主成分的方差解释比例
plot(dm_svd$d/sum(dm_svd$d), type = "b")
plot(cumsum(dm_svd$d/sum(dm_svd$d)), type = "b")
(dm_svd$d[1]+dm_svd$d[2])/sum(dm_svd$d)

eigenvec <- dm_svd$u[,1:2] # 特征向量：每个主成分的载荷向量
rownames(eigenvec) <- colnames(dm)
colnames(eigenvec) <- c("PC1","PC2")

dz <- dm%*%eigenvec # 每个样本的主成分得分：标准化后的数据*相应主成分的载荷向量，再求和

df_eigenvec <- as.data.frame(eigenvec)
df_eigenvec$eigvec <- c(1)
df_dz <- as.data.frame(dz)
df_dz$eigvec <- c(0)

df_pr <- rbind(df_eigenvec, df_dz)

ggplot(df_pr, aes(x = PC1, y = PC2)) + 
          geom_point(aes(color = factor(eigvec), size = factor(eigvec), shape = factor(eigvec))) + 
          geom_line(aes(y = mean(PC2)), size = 1, colour = "darkgrey") + 
          geom_line(aes(x = mean(PC1)), size = 1, colour = "darkgrey") +
          geom_text(aes(label = rownames(df_pr)))+labs(title = "Principal Component Analysis")
