rm(list = ls())
gc()

data_divide <- function(df, divide_1 = 0.6, divide_2 = 0.5){
          idx <- sample(1:nrow(df), nrow(df)*divide_1)
          train <- df[idx,]
          train_not <- df[-idx,]
          
          idx_2 <- sample(1:nrow(train_not), nrow(train_not)*divide_2)
          valid <- train_not[idx_2,]
          test <- train_not[-idx_2,]
          
          res <- list(train = train, valid = valid, test = test)
          return(res)
} # 将df拆分为train, valid, test

train <- data_divide(df = swiss, divide_1 = 0.6, divide_2 = 0.5)$train
valid <- data_divide(df = swiss, divide_1 = 0.6, divide_2 = 0.5)$valid
test <- data_divide(df = swiss, divide_1 = 0.6, divide_2 = 0.5)$test

head(swiss)
y_train <- train[,1]
X_train <- as.matrix(cbind(b0 = c(1), train[,2:ncol(train)]))
y_valid <- valid[,1]
X_valid <- as.matrix(cbind(b0 = c(1), valid[,2:ncol(valid)]))
y_test <- test[,1]
X_test <- as.matrix(cbind(b0 = c(1), test[,2:ncol(test)]))

lin_ridge_reg <- function(X_train, y_train, X_valid, y_valid, X_test, y_test){
          if(det(t(X_train)%*%X_train) == 0){stop("The matrix is singular, can't do inverse!")}
          
          lam <- exp(1)^seq(from = 10, to = -4, length.out = 200)
          n <- length(lam); p <- ncol(X_train)
          w <- matrix(data = rep(0,n*p), nrow = n, ncol = p, byrow = T) # 初始化w矩阵
          # 对lambda进行迭代计算，得到w矩阵
          for(i in 1:length(lam)){
                    w[i,] <- t(solve(t(X_train)%*%X_train+diag(ncol(X_train))*lam[i]) %*% t(X_train) %*% y_train)
          }
          pred_valid <- X_valid %*% t(w) # 计算valid预测值
          # 在valid中计算误差最小的lambda，得到lam_final， w_final
          err_valid <- c()
          for(j in 1:ncol(pred_valid)){
                    err_valid[j] <- sum((y_valid-pred_valid[,j])^2)/length(y_valid)
          }
          err_valid_final <- min(err_valid)
          lam_final <- lam[which.min(err_valid)]
          w_final <- w[which.min(err_valid),]
          # 计算test预测值及test误差
          pred_test <- X_test %*% w_final
          err_test <- sum((y_test-pred_test)^2)/length(y_test)
          
          res <- data.frame(err_valid = err_valid_final,
                            err_test = err_test,
                            lambda = lam_final)
          
          return(res)
}
lin_ridge_reg(X_train = X_train, y_train = y_train, X_valid = X_valid, y_valid = y_valid, X_test = X_test, y_test = y_test) 
