rm(list = ls());gc()

data_divide <- function(df, divide_1 = 0.8){
          df <- df[sample(1:nrow(df), nrow(df)), ]
          idx <- sample(1:nrow(df), nrow(df)*divide_1)
          train <- df[idx,]
          test <- df[-idx,]
          res <- list(train = train, test = test)
          return(res)
}

tanh <- function(x){2/(1+exp(-x))-1}

df <- swiss
df$Fertility2 <- ifelse(df$Fertility >= mean(df$Fertility), 1, -1)
df$Fertility <- NULL

mydata <- data_divide(df = df, divide_1 = 0.7)
train <- mydata$train
test <- mydata$test

X_train <- cbind(b0 = c(1), scale(train[,1:10]))
y_train <- train[,11]

X_test <- cbind(b0 = c(1), scale(test[,1:10]))
y_test <- test[,11]

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
                    n <- 50000; p <- ncol(layer)
                    w6_iter <- rbind(w6[j,], matrix(data = rep(0,n*p), nrow = n, ncol = p, byrow = T))
                    e_in <- c(30); yita <- 0.001
                    
                    for(i in 2:nrow(w6_iter)){
                              w6_iter[i,] <- w6_iter[i-1,] - yita*t(-(y_train-y_hat)*(2*exp(-layer%*%w6_iter[i-1,])/(1+exp(-layer%*%w6_iter[i-1,]))^2))%*%layer
                              e_in[i] <- sum((y_train-tanh(layer%*%w6_iter[i,]))^2)/2
                              if(round(e_in[i],7) == round(e_in[i-1],7)){break}
                    }
                    plot(e_in)
                    w6 <- rbind(w6, w6_iter[which.min(e_in),])
                    
                    # 5.hidden_update
                    d_hddn <- (-(y_train-y_hat)*(2*exp(-layer%*%w6[j+1,])/(1+exp(-layer%*%w6[j+1,]))^2))%*%w6[j+1,]
                    
                    d_hddn1 <- d_hddn[,2]
                    d_hddn2 <- d_hddn[,3]
                    d_hddn3 <- d_hddn[,4]
                    d_hddn4 <- d_hddn[,5]
                    d_hddn5 <- d_hddn[,6]
                    
                    # w1_update
                    n <- 50000; p <- ncol(X_train)
                    w1_iter <- rbind(w1[j,], matrix(data = rep(0,n*p), nrow = n, ncol = p, byrow = T))
                    e_in <- c(30); yita <- 0.001
                    
                    for(i in 2:nrow(w1_iter)){
                              w1_iter[i,] <- w1_iter[i-1,] - yita*t(-(hddn1-d_hddn1)*(2*exp(-X_train%*%w1_iter[i-1,])/(1+exp(-X_train%*%w1_iter[i-1,]))^2))%*%X_train
                              e_in[i] <- sum((hddn1-tanh(X_train%*%w1_iter[i,]))^2)/2
                              if(round(e_in[i],7) == round(e_in[i-1],7)){break}
                    }
                    plot(e_in)
                    w1 <- rbind(w1, w1_iter[which.min(e_in),])
                    
                    # 6.w2_update
                    n <- 50000; p <- ncol(X_train)
                    w2_iter <- rbind(w2[j,], matrix(data = rep(0,n*p), nrow = n, ncol = p, byrow = T))
                    e_in <- c(30); yita <- 0.001
                    
                    for(i in 2:nrow(w2_iter)){
                              w2_iter[i,] <- w2_iter[i-1,] - yita*t(-(hddn2-d_hddn2)*(2*exp(-X_train%*%w2_iter[i-1,])/(1+exp(-X_train%*%w2_iter[i-1,]))^2))%*%X_train
                              e_in[i] <- sum((hddn2-tanh(X_train%*%w2_iter[i,]))^2)/2
                              if(round(e_in[i],7) == round(e_in[i-1],7)){break}
                    }
                    plot(e_in)
                    w2 <- rbind(w2, w2_iter[which.min(e_in),])
                    
                    # 6.w3_update
                    n <- 50000; p <- ncol(X_train)
                    w3_iter <- rbind(w3[j,], matrix(data = rep(0,n*p), nrow = n, ncol = p, byrow = T))
                    e_in <- c(30); yita <- 0.001
                    
                    for(i in 2:nrow(w3_iter)){
                              w3_iter[i,] <- w3_iter[i-1,] - yita*t(-(hddn3-d_hddn3)*(2*exp(-X_train%*%w3_iter[i-1,])/(1+exp(-X_train%*%w3_iter[i-1,]))^2))%*%X_train
                              e_in[i] <- sum((hddn3-tanh(X_train%*%w3_iter[i,]))^2)/2
                              if(round(e_in[i],7) == round(e_in[i-1],7)){break}
                    }
                    plot(e_in)
                    w3 <- rbind(w3, w3_iter[which.min(e_in),])
                    
                    # 6.w4_update
                    n <- 50000; p <- ncol(X_train)
                    w4_iter <- rbind(w4[j,], matrix(data = rep(0,n*p), nrow = n, ncol = p, byrow = T))
                    e_in <- c(30); yita <- 0.001
                    
                    for(i in 2:nrow(w4_iter)){
                              w4_iter[i,] <- w4_iter[i-1,] - yita*t(-(hddn4-d_hddn4)*(2*exp(-X_train%*%w4_iter[i-1,])/(1+exp(-X_train%*%w4_iter[i-1,]))^2))%*%X_train
                              e_in[i] <- sum((hddn4-tanh(X_train%*%w4_iter[i,]))^2)/2
                              if(round(e_in[i],7) == round(e_in[i-1],7)){break}
                    }
                    plot(e_in)
                    w4 <- rbind(w4, w4_iter[which.min(e_in),])
                    
                    # 6.w5_update
                    n <- 50000; p <- ncol(X_train)
                    w5_iter <- rbind(w5[j,], matrix(data = rep(0,n*p), nrow = n, ncol = p, byrow = T))
                    e_in <- c(30); yita <- 0.001
                    
                    for(i in 2:nrow(w5_iter)){
                              w5_iter[i,] <- w5_iter[i-1,] - yita*t(-(hddn5-d_hddn5)*(2*exp(-X_train%*%w5_iter[i-1,])/(1+exp(-X_train%*%w5_iter[i-1,]))^2))%*%X_train
                              e_in[i] <- sum((hddn5-tanh(X_train%*%w5_iter[i,]))^2)/2
                              if(round(e_in[i],7) == round(e_in[i-1],7)){break}
                    }
                    plot(e_in)
                    w5 <- rbind(w5, w5_iter[which.min(e_in),])
                    
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
          res <- list(result_summary = result_summary, 
                      best_w1 = w1[best_k,],
                      best_w2 = w2[best_k,],
                      best_w3 = w3[best_k,],
                      best_w4 = w4[best_k,],
                      best_w5 = w5[best_k,],
                      best_w6 = w6[best_k,]) # 8
          return(res)
}
