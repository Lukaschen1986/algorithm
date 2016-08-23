rm(list = ls())
gc()

data_divide <- function(df, divide_1 = 0.6){
          idx <- sample(1:nrow(df), nrow(df)*divide_1)
          train <- df[idx,]
          test <- df[-idx,]
          
          res <- list(train = train, test = test)
          return(res)
}

df <- mtcars
df$am2 <- ifelse(df$am == 0, -1, 1)

train <- data_divide(df = df, divide_1 = 0.6)$train
test <- data_divide(df = df, divide_1 = 0.6)$test

y_train <- train$am2
X_train <- as.matrix(cbind(b0 = c(1), train[,1:5]))
K_train <- (1 + 0.0001*X_train%*%t(X_train))^2

y_test <- test$am2
X_test <- as.matrix(cbind(b0 = c(1), test[,1:5]))
K_test <- (1 + 0.0001*X_test%*%t(X_train))^2

vctr_idnt <- function(x){x/sqrt(sum(x^2))}

n <- 50000; p <- ncol(K_train)
b <- matrix(data = rep(0,n*p), nrow = n, ncol = p, byrow = T)
e_in <- c(1); yita <- 0.001; lam <- 0.01; # lam <- seq(from = 0.01, to = 10, by = 0.5)

for(i in 2:nrow(b)){
          b[i,] <- b[i-1,] - yita*t(vctr_idnt((t(1/(1+exp(y_train*K_train%*%b[i-1,])))%*%(-y_train*K_train)+lam*b[i-1,]%*%K_train)*(2/nrow(K_train))))
          e_in[i] <- sum(log(1+exp(-y_train*K_train%*%b[i,])))*(2/nrow(K_train))+(t(b[i,])%*%K_train%*%b[i,])*(lam/nrow(K_train))
          if(round(e_in[i],7) == round(e_in[i-1],7)){break}
}

plot(e_in, type = "b")
b_hat <- b[which.min(e_in),]

p_hat_train <- 1/(1+exp(-K_train%*%b_hat))
y_hat_train <- ifelse(p_hat_train >= 0.5, 1, -1)
err_train <- sum(y_hat_train == y_train)/length(y_train)

p_hat_test <- 1/(1+exp(-K_test%*%b_hat))
y_hat_test <- ifelse(p_hat_test >= 0.5, 1, -1)
err_test <- sum(y_hat_test == y_test)/length(y_test)

err_train; err_test
