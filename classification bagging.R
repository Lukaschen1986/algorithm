vctr_idnt <- function(x){x/sqrt(sum(x^2))}

bagging_func <- function(train, iter){
          # iter = 5
          idx <- c(); y_train <- list(); X_train <- list(); w_hat <- c()
          for(i in 1:iter){
                    # i = 5
                    idx <- rbind(idx, sample(1:nrow(train), nrow(train), replace = T))
                    y_train[[i]] <- train[idx[i,],c("is_loss")]
                    X_train[[i]] <- as.matrix(cbind(b0 = c(1), 
                                                    scale(train[idx[i,], c("order_sum",
                                                                           "amount_sum",
                                                                           "bn_sum",
                                                                           "nums_sum",
                                                                           "unitmult_sum",
                                                                           "sensitivity_score",
                                                                           "unitmult_bao_sum",
                                                                           "cart_sum",
                                                                           "point",
                                                                           "experience",
                                                                           "netage")]), 
                                                    category_dumm_1 = train$category_dumm_1[idx[i,]],
                                                    category_dumm_2 = train$category_dumm_2[idx[i,]],
                                                    category_dumm_3 = train$category_dumm_3[idx[i,]],
                                                    categroup_dumm_1 = train$categroup_dumm_1[idx[i,]],
                                                    categroup_dumm_2 = train$categroup_dumm_2[idx[i,]],
                                                    categroup_dumm_3 = train$categroup_dumm_3[idx[i,]],
                                                    categroup_dumm_4 = train$categroup_dumm_4[idx[i,]],
                                                    lv_id_dumm_1 = train$lv_id_dumm_1[idx[i,]],
                                                    lv_id_dumm_2 = train$lv_id_dumm_2[idx[i,]],
                                                    lv_id_dumm_3 = train$lv_id_dumm_3[idx[i,]],
                                                    lv_id_dumm_4 = train$lv_id_dumm_4[idx[i,]]))
                    
                    n <- 50000; p <- ncol(X_train[[i]])
                    w <- matrix(data = rep(0,n*p), nrow = n, ncol = p, byrow = T)
                    e_in <- c(1); yita <- 0.001 # 0.01, 0.03, 0.1, 0.3, 1, 3, 10
                    
                    for(k in 2:nrow(w)){
                              w[k,] <- w[k-1,] - yita*t(vctr_idnt(t(1/(1+exp(y_train[[i]]*X_train[[i]]%*%w[k-1,])))%*%(-y_train[[i]]*X_train[[i]])/nrow(X_train[[i]])))
                              e_in[k] <- sum(log(1+exp(-y_train[[i]]*X_train[[i]]%*%w[k,])))/nrow(X_train[[i]])
                              if(round(e_in[k],7) == round(e_in[k-1],7)){break}
                    }
                    plot(e_in, type = "b")
                    w_hat <- rbind(w_hat, w[which.min(e_in),])
          }
          
          p_hat_test <- c(); y_pred_test <- c()
          for(i in 1:iter){
                    p_hat_test <- cbind(p_hat_test, 1/(1+exp(-X_test%*%w_hat[i,])))
          }
          p_hat_test_final <- apply(p_hat_test,1,mean)
          y_pred_test <- ifelse(p_hat_test_final >= (0.5+mean(p_hat_test_final))/2, 1, -1)
          # y_pred_test <- ifelse(p_hat_test_final >= 0.5, 1, -1)
          # y_pred_test <- ifelse(p_hat_test_final >= median(p_hat_test_final), 1, -1)
          accu_test <- sum(y_pred_test == y_test)/length(y_test)
          
          res <- list(p_hat_test_final = p_hat_test_final, y_pred_test = y_pred_test, w_hat = w_hat, accu_test = accu_test)
          return(res)
}
