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

y_train <- train[,1]
X_train <- as.matrix(cbind(b0 = c(1), train[,2:ncol(train)]))
K_train <- (5 + 0.0001*X_train%*%t(X_train))^2

y_valid <- valid[,1]
X_valid <- as.matrix(cbind(b0 = c(1), valid[,2:ncol(valid)]))
K_valid <- (5 + 0.0001*X_valid%*%t(X_train))^2

y_test <- test[,1]
X_test <- as.matrix(cbind(b0 = c(1), test[,2:ncol(test)]))
K_test <- (5 + 0.0001*X_test%*%t(X_train))^2

support_vector_reg <- function(K_train, y_train, K_valid, y_valid, K_test, y_test){
          if(det(K_train) == 0){stop("The matrix is singular, can't do inverse!")}
          
          lam <- exp(1)^seq(from = 10, to = -4, length.out = 200)
          n <- length(lam); p <- ncol(K_train)
          b <- matrix(data = rep(0,n*p), nrow = n, ncol = p, byrow = T) # 初始化b矩阵
          # 对lambda进行迭代计算，得到b矩阵
          for(i in 1:length(lam)){
                    b[i,] <- t(solve(K_train+diag(ncol(K_train))*lam[i])%*%y_train)
          }
          pred_valid <- K_valid %*% t(b) # 计算valid预测值
          # 在valid中计算误差最小的lambda，得到lam_final， b_final
          err_valid <- c()
          for(j in 1:ncol(pred_valid)){
                    err_valid[j] <- sum((y_valid-pred_valid[,j])^2)/length(y_valid)
          }
          err_valid_final <- min(err_valid)
          lam_final <- lam[which.min(err_valid)]
          b_final <- b[which.min(err_valid),]
          # 计算test预测值及test误差
          pred_test <- K_test %*% b_final
          err_test <- sum((y_test-pred_test)^2)/length(y_test)
          
          res <- data.frame(err_valid = err_valid_final,
                            err_test = err_test,
                            lambda = lam_final)
          
          return(res)
}
support_vector_reg(K_train = K_train, y_train = y_train, K_valid = K_valid, y_valid = y_valid, K_test = K_test, y_test = y_test)
