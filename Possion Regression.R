rm(list = ls())
gc()

mtcars$carb <- as.integer(mtcars$carb)

data_divide <- function(df, divide_1 = 0.6){
          idx <- sample(1:nrow(df), nrow(df)*divide_1)
          train <- df[idx,]
          test <- df[-idx,]
          
          res <- list(train = train, test = test)
          return(res)
}
df <- data_divide(df = mtcars, divide_1 = 0.8)
train <- df$train
test <- df$test

y_train <- train$carb
X_train <- as.matrix(cbind(b0 = c(1), train[,1:6]))

y_test <- test$carb
X_test <- as.matrix(cbind(b0 = c(1), test[,1:6]))

vctr_idnt <- function(x){x/sqrt(sum(x^2))}
if(det(t(X_train)%*%X_train) == 0){stop("The matrix is singular, can't do inverse!")}
w_lin <- solve(t(X_train)%*%X_train) %*% t(X_train) %*% y_train

n <- 50000; p <- ncol(X_train)
w <- rbind(t(w_lin), matrix(data = rep(0,n*p), nrow = n, ncol = p, byrow = T)) 
e_in <- c(2000); yita <- 0.001 # 0.01, 0.03, 0.1, 0.3, 1, 3, 10

for(i in 2:nrow(w)){
          w[i,] <- w[i-1,] - yita*vctr_idnt(-t(y_train)%*%X_train+t(exp(X_train%*%w[i-1,]))%*%X_train)
          e_in[i] <- -sum(y_train*X_train%*%w[i,] - log(factorial(y_train)) - exp(X_train%*%w[i,]))
          if(round(e_in[i],7) == round(e_in[i-1],7)){break}
}
plot(e_in, type = "b")
w_hat <- w[which.min(e_in),] 
lam_hat <- round(exp(X_test%*%w_hat))
sum(lam_hat == y_test)/length(y_test)
