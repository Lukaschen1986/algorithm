library(dplyr); library(proxy)
rm(list = ls()); gc()

df <- swiss
df$y[df[,1] < mean(df[,1])] <- -1
df$y[df[,1] >= mean(df[,1])] <- 1
df <- df[,-1]

data_divide <- function(df, divide_1){
          idx <- sample(1:nrow(df), nrow(df)*divide_1)
          train <- df[idx,]
          test <- df[-idx,]
          
          res <- list(train = train, test = test)
          return(res)
}
df_2 <- data_divide(df = df, divide_1 = 0.8)
train <- df_2$train
test <- df_2$test

# training
n_train <- nrow(train); p <- ncol(train); res <- c()

for(k in 3:20){ # 设定k的个数
          # k = 3
          l <- ceiling(k*0.6) # l的个数
          y_pred <- c() 
          
          for(i in 1:n_train){
                    a <- train[i, ] # 依次取训练集中样本
                    # a <- train[1, ]
                    d <- dist(x = a[,1:(p-1)], y = train[,1:(p-1)], method = "euclidean") # 计算与所有样本的距离
                    d_2 <- data.frame(cbind(as.numeric(d),train$y))
                    names(d_2) <- c("dist","y")
                    
                    d_2_arrange <- arrange(d_2, dist) # 升序排序
                    d_2_k <- d_2_arrange[2:(k+1),] # 取与样本距离最近的k个样本
                    d_2_tb <- data.frame(table(d_2_k[,2])) # 统计k个样本标记值个数
                    names(d_2_tb) <- c("y","num")
                    
                    b <- subset(d_2_tb, num >= l & num == max(num))[1] # 根据下限l和上限max过滤标记值
                    y_pred <- rbind(y_pred, b) # 输出预测标记
          }
          acc <- sum(y_pred == train$y)/length(train$y) # 计算误差
          res <- rbind(res, acc)
}
res <- cbind(res, k = c(3:20))

res <- data.frame(res)
names(res) <- c("acc","k")
k_best <- res$k[which.max(res$acc)]
l_best <- ceiling(k_best*0.6)

# testing
n_test <- nrow(test); y_pred_test <- c()

for(j in 1:n_test){
          a <- test[j, ] # 依次取测试集中样本
          d <- dist(x = a[,1:(p-1)], y = train[,1:(p-1)], method = "euclidean") # 计算与所有训练样本的距离
          d_2 <- data.frame(cbind(as.numeric(d),train$y))
          names(d_2) <- c("dist","y")
          
          d_2_arrange <- arrange(d_2, dist) # 升序排序
          d_2_k <- d_2_arrange[1:k_best,] # 取与样本距离最近的k个样本
          d_2_tb <- data.frame(table(d_2_k[,2])) # 统计k个样本标记值个数
          names(d_2_tb) <- c("y","num")
          
          b <- subset(d_2_tb, num >= l_best & num == max(num))[1] # 根据下限l和上限max过滤标记值
          y_pred_test <- rbind(y_pred_test, b) # 输出预测标记
}
acc_test <- sum(y_pred_test == test$y)/length(test$y) # 计算误差
