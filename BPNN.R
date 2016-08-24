rm(list = ls());gc()

# func
data_divide <- function(df, divide_1 = 0.8){
          df <- df[sample(1:nrow(df), nrow(df)), ]
          idx <- sample(1:nrow(df), nrow(df)*divide_1)
          train <- df[idx,]
          test <- df[-idx,]
          res <- list(train = train, test = test)
          return(res)
}

tanh <- function(x){2/(1+exp(-x))-1}

logit_reg <- function(n_prmtr, w_prmtr, e_in, yita, y_1, y_2, data){
          n <- n_prmtr; p <- ncol(data)
          w_iter <- rbind(w_prmtr[j,], matrix(data = rep(0,n*p), nrow = n, ncol = p, byrow = T))
          
          for(i in 2:nrow(w_iter)){
                    w_iter[i,] <- w_iter[i-1,] - yita*t(-(y_1-y_2)*(2*exp(-data%*%w_iter[i-1,])/(1+exp(-data%*%w_iter[i-1,]))^2))%*%data
                    e_in[i] <- sum((y_1-tanh(data%*%w_iter[i,]))^2)/2
                    if(round(e_in[i],7) == round(e_in[i-1],7)){break}
          }
          res <- list(w_iter = w_iter, e_in = e_in)
          return(res)
}

# data
df <- swiss
df$Fertility2 <- ifelse(df$Fertility >= mean(df$Fertility), 1, -1)
df$Fertility <- NULL

mydata <- data_divide(df = df, divide_1 = 0.7)
train <- mydata$train
test <- mydata$test

X_train <- cbind(b0 = c(1), scale(train[,1:5]))
y_train <- train[,6]

X_test <- cbind(b0 = c(1), scale(test[,1:5]))
y_test <- test[,6]

# BPNN
bpnn_func <- function(X_train, y_train, X_test, y_test, k){
          w1 <- c(); w2 <- c(); w3 <- c(); w4 <- c(); w5 <- c(); w6 <- c(); y_hat <- c()
          sse_train <- c(); accu_train <- c(); sse_test <- c(); accu_test <- c()
          
          # 1
          w1 <- rbind(w1, rnorm(n = ncol(X_train), mean = 0, sd = 1))
          w2 <- rbind(w2, rnorm(n = ncol(X_train), mean = 0, sd = 1))
          w3 <- rbind(w3, rnorm(n = ncol(X_train), mean = 0, sd = 1))
          w4 <- rbind(w4, rnorm(n = ncol(X_train), mean = 0, sd = 1))
          w5 <- rbind(w5, rnorm(n = ncol(X_train), mean = 0, sd = 1))
          w6 <- rbind(w6, rnorm(n = 6, mean = 0, sd = 1))
          
          for(j in 1:k){
                    # j = 1
                    hddn1 <- tanh(X_train%*%w1[j,])
                    hddn2 <- tanh(X_train%*%w2[j,])
                    hddn3 <- tanh(X_train%*%w3[j,])
                    hddn4 <- tanh(X_train%*%w4[j,])
                    hddn5 <- tanh(X_train%*%w5[j,])
                    layer <- cbind(b0 = c(1), hddn1, hddn2, hddn3, hddn4, hddn5) # 2
                    y_hat <- tanh(layer%*%w6[j,]) # 3
                    
                    # 4.w6_update
                    res6 <- logit_reg(n_prmtr = 50000, w_prmtr = w6, e_in = 30, yita = 0.001, y_1 = y_train, y_2 = y_hat, data = layer)
                    plot(res6$e_in)
                    w6 <- rbind(w6, res6$w_iter[which.min(res6$e_in),])
                    
                    # 5.hidden_update
                    d_hddn <- (-(y_train-y_hat)*(2*exp(-layer%*%w6[j+1,])/(1+exp(-layer%*%w6[j+1,]))^2))%*%w6[j+1,]
                    
                    d_hddn1 <- d_hddn[,2]
                    d_hddn2 <- d_hddn[,3]
                    d_hddn3 <- d_hddn[,4]
                    d_hddn4 <- d_hddn[,5]
                    d_hddn5 <- d_hddn[,6]
                    
                    # w1_update
                    res1 <- logit_reg(n_prmtr = 50000, w_prmtr = w1, e_in = 30, yita = 0.001, y_1 = hddn1, y_2 = d_hddn1, data = X_train)
                    plot(res1$e_in)
                    w1 <- rbind(w1, res1$w_iter[which.min(res1$e_in),])
                    
                    # 6.w2_update
                    res2 <- logit_reg(n_prmtr = 50000, w_prmtr = w2, e_in = 30, yita = 0.001, y_1 = hddn2, y_2 = d_hddn2, data = X_train)
                    plot(res2$e_in)
                    w2 <- rbind(w2, res2$w_iter[which.min(res2$e_in),])
                    
                    # 6.w3_update
                    res3 <- logit_reg(n_prmtr = 50000, w_prmtr = w3, e_in = 30, yita = 0.001, y_1 = hddn3, y_2 = d_hddn3, data = X_train)
                    plot(res3$e_in)
                    w3 <- rbind(w3, res3$w_iter[which.min(res3$e_in),])
                    
                    # 6.w4_update
                    res4 <- logit_reg(n_prmtr = 50000, w_prmtr = w4, e_in = 30, yita = 0.001, y_1 = hddn4, y_2 = d_hddn4, data = X_train)
                    plot(res4$e_in)
                    w4 <- rbind(w4, res4$w_iter[which.min(res4$e_in),])
                    
                    # 6.w5_update
                    res5 <- logit_reg(n_prmtr = 50000, w_prmtr = w5, e_in = 30, yita = 0.001, y_1 = hddn5, y_2 = d_hddn5, data = X_train)
                    plot(res5$e_in)
                    w5 <- rbind(w5, res5$w_iter[which.min(res5$e_in),])
                    
                    # 7.pred
                    y_pred <- tanh(cbind(b0 = c(1), 
                                         tanh(X_train%*%w1[j+1,]), 
                                         tanh(X_train%*%w2[j+1,]), 
                                         tanh(X_train%*%w3[j+1,]), 
                                         tanh(X_train%*%w4[j+1,]), 
                                         tanh(X_train%*%w5[j+1,])) 
                                   %*% w6[j+1,])
                    sse_train <- rbind(sse_train, sum((y_train-y_pred)^2)/2)
                    accu_train <- rbind(accu_train, sum(y_train == ifelse(y_pred >= 0, 1, -1))/length(y_train))
                    
                    y_pred_test <- tanh(cbind(b0 = c(1), 
                                              tanh(X_test%*%w1[j+1,]), 
                                              tanh(X_test%*%w2[j+1,]), 
                                              tanh(X_test%*%w3[j+1,]), 
                                              tanh(X_test%*%w4[j+1,]), 
                                              tanh(X_test%*%w5[j+1,])) 
                                        %*% w6[j+1,])
                    sse_test <- rbind(sse_test, sum((y_test-y_pred_test)^2)/2)
                    accu_test <- rbind(accu_test, sum(y_test == ifelse(y_pred_test >= 0, 1, -1))/length(y_test))
          }
          result_summary <- data.frame(sse_train = sse_train, accu_train = accu_train, sse_test = sse_test, accu_test = accu_test)
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

sse_train; accu_train; sse_test; accu_test
