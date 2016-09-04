rm(list = ls());gc()

# func
data_divide <- function(df, divide_1 = 0.8){
          idx <- sample(1:nrow(df), nrow(df)*divide_1)
          train <- df[idx,]
          test <- df[-idx,]
          
          res <- list(train = train, test = test)
          return(res)
}

# data
mydata <- data_divide(df = df, divide_1 = 0.8)
train <- mydata$train
test <- mydata$test

y_train <- train[,6]
X_train <- cbind(b0 = c(1), scale(train[,1:5]))

y_test <- test[,6]
X_test <- cbind(b0 = c(1), (test[,1:5]-apply(train[,1:5],2,mean))/apply(train[,1:5],2,sd))

# BPNN
bpnn_func <- function(X_train, y_train, X_test, y_test, k){
          tanh <- function(x){2/(1+exp(-x))-1}
          vctr_idnt <- function(x){x/sqrt(sum(x^2))}
          
          grdnt_dscnt <- function(n_prmtr, w_prmtr, e_in, yita, y_1, y_2, data){
                    n <- n_prmtr; p <- ncol(data)
                    w_iter <- rbind(w_prmtr[j,], matrix(data = rep(0,n*p), nrow = n, ncol = p, byrow = T))
                    for(i in 2:nrow(w_iter)){
                              w_iter[i,] <- w_iter[i-1,] - yita*vctr_idnt(t(-(y_1-y_2)*(2*exp(-data%*%w_iter[i-1,])/(1+exp(-data%*%w_iter[i-1,]))^2))%*%data)
                              e_in[i] <- sum((y_1-tanh(data%*%w_iter[i,]))^2)/2
                              if(round(e_in[i],7) == round(e_in[i-1],7)){break}
                    }
                    res <- list(w_iter = w_iter, e_in = e_in)
                    return(res)
          }
          
          grdnt_dscnt_pois <- function(n_prmtr, w_prmtr, e_in, yita, y, data){
                    n <- n_prmtr; p <- ncol(data)
                    w_iter <- rbind(w_prmtr[j,], matrix(data = rep(0,n*p), nrow = n, ncol = p, byrow = T))
                    for(i in 2:nrow(w_iter)){
                              w_iter[i,] <- w_iter[i-1,] - yita*vctr_idnt(-t(y)%*%data+t(exp(data%*%w_iter[i-1,]))%*%data)
                              e_in[i] <- -sum(y*data%*%w_iter[i,] - exp(data%*%w_iter[i,]))
                              if(round(e_in[i],7) == round(e_in[i-1],7)){break}
                    }
                    res <- list(w_iter = w_iter, e_in = e_in)
                    return(res)
          }
          
          w1 <- c(); w2 <- c(); w3 <- c(); w4 <- c(); w5 <- c(); w6 <- c()
          sse_train <- c(); sse_test <- c()
          
          # 1
          w1 <- rbind(w1, rnorm(n = ncol(X_train), mean = 0, sd = 1))
          w2 <- rbind(w2, rnorm(n = ncol(X_train), mean = 0, sd = 1))
          w3 <- rbind(w3, rnorm(n = ncol(X_train), mean = 0, sd = 1))
          w4 <- rbind(w4, rnorm(n = ncol(X_train), mean = 0, sd = 1))
          w5 <- rbind(w5, rnorm(n = ncol(X_train), mean = 0, sd = 1))
          w6 <- rbind(w6, rnorm(n = 6, mean = 0, sd = 1))
          # k = 10
          for(j in 1:k){
                    # j = 3
                    hddn1 <- tanh(X_train%*%w1[j,])
                    hddn2 <- tanh(X_train%*%w2[j,])
                    hddn3 <- tanh(X_train%*%w3[j,])
                    hddn4 <- tanh(X_train%*%w4[j,])
                    hddn5 <- tanh(X_train%*%w5[j,])
                    layer <- cbind(b0 = c(1), hddn1, hddn2, hddn3, hddn4, hddn5) # 2
                    # y_hat <- exp(layer%*%w6[j,]) # 3
                    
                    # 4.w6_update
                    res6 <- grdnt_dscnt_pois(n_prmtr = 50000, w_prmtr = w6, e_in = 1000, yita = 0.0001, y = y_train, data = layer)
                    # plot(res6$e_in[-1]); min(res6$e_in[-1]); which.min(res6$e_in[-1])+1
                    w6 <- rbind(w6, res6$w_iter[which.min(res6$e_in[-1])+1,])
                    
                    # 5.hidden_update
                    d_hddn <- -y_train%*%t(w6[j+1,])+exp(layer%*%w6[j+1,])%*%t(w6[j+1,])
                    d_hddn1 <- d_hddn[,2]
                    d_hddn2 <- d_hddn[,3]
                    d_hddn3 <- d_hddn[,4]
                    d_hddn4 <- d_hddn[,5]
                    d_hddn5 <- d_hddn[,6]
                    
                    # w1_update
                    res1 <- grdnt_dscnt(n_prmtr = 50000, w_prmtr = w1, e_in = 1000, yita = 0.0001, y_1 = d_hddn1, y_2 = hddn1, data = X_train)
                    # plot(res1$e_in[-1]); min(res1$e_in[-1]); which.min(res1$e_in[-1])+1
                    w1 <- rbind(w1, res1$w_iter[which.min(res1$e_in[-1])+1,])
                    
                    # 6.w2_update
                    res2 <- grdnt_dscnt(n_prmtr = 50000, w_prmtr = w2, e_in = 1000, yita = 0.0001, y_1 = d_hddn2, y_2 = hddn2, data = X_train)
                    # plot(res2$e_in[-1]); min(res2$e_in[-1]); which.min(res2$e_in[-1])+1
                    w2 <- rbind(w2, res2$w_iter[which.min(res2$e_in[-1])+1,])
                    
                    # 6.w3_update
                    res3 <- grdnt_dscnt(n_prmtr = 50000, w_prmtr = w3, e_in = 1000, yita = 0.0001, y_1 = d_hddn3, y_2 = hddn3, data = X_train)
                    # plot(res3$e_in[-1]); min(res3$e_in); which.min(res3$e_in[-1])+1
                    w3 <- rbind(w3, res3$w_iter[which.min(res3$e_in[-1])+1,])
                    
                    # 6.w4_update
                    res4 <- grdnt_dscnt(n_prmtr = 50000, w_prmtr = w4, e_in = 1000, yita = 0.0001, y_1 = d_hddn4, y_2 = hddn4, data = X_train)
                    # plot(res4$e_in[-1]); min(res4$e_in); which.min(res4$e_in[-1])+1
                    w4 <- rbind(w4, res4$w_iter[which.min(res4$e_in[-1])+1,])
                    
                    # 6.w5_update
                    res5 <- grdnt_dscnt(n_prmtr = 50000, w_prmtr = w5, e_in = 1000, yita = 0.0001, y_1 = d_hddn5, y_2 = hddn5, data = X_train)
                    # plot(res5$e_in[-1]); min(res5$e_in); which.min(res5$e_in[-1])+1
                    w5 <- rbind(w5, res5$w_iter[which.min(res5$e_in[-1])+1,])
                    
                    # 7.pred
                    y_pred <- exp(cbind(b0 = c(1), 
                                        tanh(X_train%*%w1[j+1,]), 
                                        tanh(X_train%*%w2[j+1,]), 
                                        tanh(X_train%*%w3[j+1,]), 
                                        tanh(X_train%*%w4[j+1,]), 
                                        tanh(X_train%*%w5[j+1,])) %*% w6[j+1,])
                    y_pred <- round(y_pred)
                    
                    sse_train <- rbind(sse_train, sum(abs(y_pred-y_train)/y_train)/length(y_train))
                    
                    y_pred_test <- exp(cbind(b0 = c(1), 
                                             tanh(X_test%*%w1[j+1,]), 
                                             tanh(X_test%*%w2[j+1,]), 
                                             tanh(X_test%*%w3[j+1,]), 
                                             tanh(X_test%*%w4[j+1,]), 
                                             tanh(X_test%*%w5[j+1,])) %*% w6[j+1,])
                    y_pred_test <- round(y_pred_test)
                    
                    sse_test <- rbind(sse_test, sum(abs(y_pred_test-y_test)/y_test)/length(y_test))
                    # sse_train; sse_test
          }
          result_summary <- data.frame(sse_train = sse_train, sse_test = sse_test)
          best_k <- which.min(result_summary$sse_test)+1
          res_final <- list(result_summary = result_summary, 
                            best_w1 = w1[best_k,],
                            best_w2 = w2[best_k,],
                            best_w3 = w3[best_k,],
                            best_w4 = w4[best_k,],
                            best_w5 = w5[best_k,],
                            best_w6 = w6[best_k,]) # 8
          return(res_final)
}
res_bp <- bpnn_func(X_train = X_train, y_train = y_train, X_test = X_test, y_test = y_test, k = 20)
res_bp$result_summary
res_bp$best_w1
