rm(list = ls()); gc()

# smote
library(ROSE)
data(hacide)
train <- hacide.train
test <- hacide.test

table(train[,1])
m <- table(train[,1])[1] / table(train[,1])[2] # 每个少数类样本应该合成的新样本个数: 49
train_minority <- train[train$cls == 1, 2:3] # 筛选出少数类样本的自变量
dist_row <- as.matrix(dist(train_minority, method = "euclidean", diag = T, upper = T)) # 自变量距离矩阵

smote <- function(dist_matrix, samp_name, k, l){
          # dist_matrix <- dist_row; k = 5; l = 2; samp_name = "981"
          samp_idx <- which(rownames(dist_matrix) == samp_name) # 找到目标样本对应的行号
          samp_knn <- dist_matrix[samp_idx, ] # 找到距离样本
          knn_idx <- order(samp_knn)[2:(k+1)] # 取距离最近的k个样本
          
          k_df <- data.frame(samp_knn[knn_idx]); names(k_df) <- c("dist")
          knn_name <- row.names(k_df)[1:l] # 取前l个样本
          
          samp_similar <- train[knn_name,2:3] # 反查找到相似样本的自变量
          samp_origin <- train[samp_name,2:3] # 反查找到原样本的自变量
          
          samp_new <- c(); r_unif <- c()
          for(i in 1:l){
                    for(j in 1:round(m/l)){
                              r_unif[j] <- runif(n = 1, min = 0, max = 1)
                              samp_new <- rbind(samp_new, samp_origin + r_unif[j]*(samp_similar[i,]-samp_origin))
                    }
          }
          return(samp_new)
}
# smote(dist_matrix = dist_row, samp_name = rownames(dist_row)[5], k = 5, l = 3)

samp_name <- rownames(dist_row); res <- c()
for(i in 1:length(samp_name)){
          a <- smote(dist_matrix = dist_row, samp_name = samp_name[i], k = 10, l = 8)
          res <- rbind(res, a)
}

# df_new
df_new <- rbind(cbind(cls = c(1), res), train[,1:3])
df_new$cls[df_new$cls == 0] <- -1; df_new$cls <- as.numeric(df_new$cls)

X_train <- as.matrix(cbind(b0 = c(1), df_new[,2:3]))
y_train <- df_new[,1]

test$cls <- as.numeric(as.character(test$cls))
test$cls[test$cls == 0] <- -1

X_test <- as.matrix(cbind(b0 = c(1), test[,2:3]))
y_test <- test[,1]

# model
vctr_idnt <- function(x){x/sqrt(sum(x^2))}
n <- 50000; p <- ncol(X_train)
w <- matrix(data = rep(0,n*p), nrow = n, ncol = p, byrow = T) 
head(w)
e_in <- c(1); yita <- 0.001 # 0.01, 0.03, 0.1, 0.3, 1, 3, 10

for(i in 2:nrow(w)){
          w[i,] <- w[i-1,] - yita*t(vctr_idnt(t(1/(1+exp(y_train*X_train%*%w[i-1,])))%*%(-y_train*X_train)/nrow(X_train))) 
          e_in[i] <- sum(log(1+exp(-y_train*X_train%*%w[i,])))/nrow(X_train) 
          if(round(e_in[i],7) == round(e_in[i-1],7)){break}
}
plot(e_in, type = "b")
w_hat <- w[which.min(e_in),] 
p_hat <- 1/(1+exp(-X_test%*%w_hat)) 
y_hat <- ifelse(p_hat >= 0.5, 1, -1)
sum(y_hat == y_test)/length(y_test)
cbind(y_test, y_hat)
