bagging_func <- function(train, k, test){
          # k = 20
          idx <- c(); y_train <- list(); X_train <- list(); w_hat <- c()
          for(i in 1:k){
                    idx <- rbind(idx, sample(1:nrow(train), nrow(train), replace = T))
                    y_train[[i]] <- train[idx[i,],1]
                    X_train[[i]] <- as.matrix(cbind(b0 = c(1), train[idx[i,],2:6]))
                    if(det(t(X_train[[i]])%*%X_train[[i]]) == 0){stop("The matrix is singular, can't do inverse!")}
                    w_hat <- cbind(w_hat, solve(t(X_train[[i]])%*%X_train[[i]])%*%t(X_train[[i]])%*%y_train[[i]])
          }
          
          y_test <- test[,1]
          X_test <- as.matrix(cbind(b0 = c(1), test[,2:6]))
          
          y_pred <- c()
          for(i in 1:k){
                    y_pred <- cbind(y_pred, X_test %*% w_hat[,i])
          }
          y_pred <- apply(y_pred,1,mean)
          err <- sum((y_pred-y_test)^2)/length(y_test)
          res <- list(y_pred = y_pred, err = err)
          return(res)
}
res <- bagging_func(train = train, k = 20, test = test)
res <- list(); a <- c()
for(i in 1:10){
          res[[i]] <- bagging_func(train = train, k = 20, test = test)
          a <- rbind(a, res[[i]][2])
}
min(as.numeric(a))
res[[which.min(a)]]$y_pred
