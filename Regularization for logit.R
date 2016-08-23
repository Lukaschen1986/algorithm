vctr_idnt <- function(x){x/sqrt(sum(x^2))}

n <- 50000; p <- ncol(X_train)
w <- matrix(data = rep(0,n*p), nrow = n, ncol = p, byrow = T)
e_in <- c(1.5); yita <- 0.001; lam <- seq(from = 0.01, to = 10, by = 1)
w_hat <- matrix(data = rep(0,n*p), nrow = length(lam), ncol = p, byrow = T)

for(j in 1:length(lam)){
          for(i in 2:nrow(w)){
                    w[i,] <- w[i-1,] - yita*(t(1/(1+exp(y_train*X_train%*%w[i-1,])))%*%(-y_train*X_train)+lam[j]*w[i-1,])*(2/nrow(X_train))
                    e_in[i] <- sum(log(1+exp(-y_train*X_train%*%w[i,])))*(2/nrow(X_train))+(t(w[i,])%*%w[i,])*(lam[j]/nrow(X_train))
                    if(round(e_in[i],6) == round(e_in[i-1],6)){break}
                    w_hat[j,] <- w[which.min(e_in),]
          }
}
plot(e_in, type = "b")
p_hat_valid <- 1/(1+exp(-X_valid%*%t(w_hat)))
y_pred_valid <- ifelse(p_hat_valid > 0.5, 1, -1)
y_pred_valid <- ifelse(p_hat_valid > mean(p_hat_valid), 1, -1)

accu_valid <- c()
for(i in 1:ncol(y_pred_valid)){
          accu_valid[i] <- sum(y_pred_valid[,i] == y_valid)/length(y_valid)
}
accu_valid_final <- max(accu_valid)
lam_final <- lam[which.max(accu_valid)]
w_final <- w_hat[which.max(accu_valid),]

p_hat_test <- 1/(1+exp(-X_test%*%w_final))
y_pred_test <- ifelse(p_hat_test > 0.5, 1, -1)
y_pred_test <- ifelse(p_hat_test > mean(p_hat_test), 1, -1)
sum(y_pred_test == y_test)/length(y_test)
table(y_test, y_pred_test)
