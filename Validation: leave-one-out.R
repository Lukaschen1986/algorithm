loocv_valid <- function(df){
          res_train <- rep(0,nrow(df))
          res_valid <- rep(0,nrow(df))
          
          for(i in 1:nrow(df)){
                    train <- df[-i,]
                    valid <- df[i,]
                    
                    X_train <- as.matrix(cbind(b0 = c(1), train[,2:ncol(train)]))
                    y_train <- train[,1]
                    
                    X_valid <- as.matrix(cbind(b0 = c(1), valid[,2:ncol(valid)]))
                    y_valid <- valid[,1]
                    
                    w <- solve(t(X_train)%*%X_train) %*% t(X_train) %*% y_train
                    pred_train <- X_train %*% w
                    pred_valid <- X_valid %*% w
                    
                    res_train[i] <- mean((y_train-pred_train)^2)
                    res_valid[i] <- (y_valid-pred_valid)^2
          }
          res <- list(res_train = res_train, res_valid = res_valid)
          return(res)
}
mean(loocv_valid(df = swiss)$res_train)
mean(loocv_valid(df = swiss)$res_valid)
