vctr_idnt <- function(x){x/sqrt(sum(x^2))}

adaboost_func <- function(X_train, X_test, iter){
          # iter = 5
          w_hat <- c(); a <- c(); e_in_step <- c(); err <- c(); a <- c()
          wt <- cbind(rep(1/nrow(X_train), nrow(X_train)))
          ## training
          # i = 5
          for(i in 1:iter){
                    X_train_wt <- c()
                    for(j in 1:nrow(X_train)){
                              X_train_wt <- rbind(X_train_wt, X_train[j,]*wt[j,i])
                    }
                    
                    n <- 50000; p <- ncol(X_train)
                    w <- matrix(data = rep(0,n*p), nrow = n, ncol = p, byrow = T)
                    e_in <- c(1); yita <- 0.01
                    for(k in 2:nrow(w)){
                              w[k,] <- w[k-1,] - yita*t(vctr_idnt(t(1/(1+exp(y_train*X_train_wt%*%w[k-1,])))%*%(-y_train*X_train_wt)/nrow(X_train_wt)))
                              e_in[k] <- sum(log(1+exp(-y_train*X_train_wt%*%w[k,])))/nrow(X_train_wt)
                              if(round(e_in[k],7) == round(e_in[k-1],7)){break}
                    }
                    plot(e_in, type = "b")
                    e_in_step <- rbind(e_in_step, min(e_in)) #; 
                    
                    w_hat <- rbind(w_hat, w[which.min(e_in),])
                    p_hat <- 1/(1+exp(-X_train_wt%*%w_hat[i,]))
                    
                    y_hat <- ifelse(p_hat >= mean(p_hat), 1, -1)
                    # y_hat <- ifelse(p_hat >= 0.5, 1, -1)
                    err <- rbind(err, sum(y_hat != y_train)/length(y_train)) #; err
                    a <- rbind(a, 0.5*log((1-err[i])/err[i])) #; a
                    z <- sum(wt[,i]*exp(-a[i,]*y_train*y_hat))
                    wt <- cbind(wt, (wt[,i]/z)*exp(-a[i,]*y_train*y_hat))
          }
          ## testing
          y_pred <- c(); y_pred_2 <- c()
          for(i in 1:iter){
                    # i = 5
                    # iter = 5
                    p_hat <- 1/(1+exp(-X_test%*%w_hat[i,]))
                    y_pred <- cbind(y_pred, ifelse(p_hat >= mean(p_hat), 1, -1))
                    # y_pred <- cbind(y_pred, ifelse(p_hat >= 0.5, 1, -1))
                    y_pred_2 <- cbind(y_pred_2, y_pred[,i]*a[i,])
          }
          y_pred_final <- sign(apply(y_pred_2, 1, sum))
          accu <- sum(y_pred_final == y_test)/length(y_test)
          res <- list(total_accu = accu, e_in_step = e_in_step, err_step = err, w_hat = w_hat, a = a)
          return(res)
}
res <- adaboost_func(X_train = X_train, X_test = X_test, iter = 3)
res$total_accu # 0.7835325
res$e_in_watch; plot(res$e_in_watch)
res$err_step; plot(res$err_step)
res$a; plot(res$a)
