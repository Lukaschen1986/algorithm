k_fold_valid <- function(df, cross = 5){
          if(cross == 1){stop("The cross could not be 1!")}
          if(nrow(df)/cross < 30){print("The result may be not very specific!")}
          
          ind_1 <- sample(1:nrow(df), nrow(df))
          ind_2 <- rep(1:cross, ceiling(nrow(df)/cross))[ind_1]
          
          res_train <- rep(0,cross)
          res_valid <- rep(0,cross)
          
          for(i in 1:cross){
                    valid_ind <- ind_1[ind_2 == i]
                    train <- df[-valid_ind,]
                    valid <- df[valid_ind,]
                    
                    X_train <- as.matrix(cbind(b0 = c(1), train[,2:ncol(train)]))
                    y_train <- train[,1]
                    
                    X_valid <- as.matrix(cbind(b0 = c(1), valid[,2:ncol(valid)]))
                    y_valid <- valid[,1]
                    
                    w <- solve(t(X_train)%*%X_train) %*% t(X_train) %*% y_train
                    pred_train <- X_train %*% w
                    pred_valid <- X_valid %*% w
                    
                    res_train[i] <- mean((y_train-pred_train)^2)
                    res_valid[i] <- mean((y_valid-pred_valid)^2)
          }
          res <- list(res_train = res_train, res_valid = res_valid)
          return(res)
}
mean(k_fold_valid(df = swiss, cross = 5)$res_train)
mean(k_fold_valid(df = swiss, cross = 5)$res_valid)
