# Polynomial kernel function
X_train <- as.matrix(cbind(b0 = c(1), train[,c("disp","drat","wt","qsec")]))
K_train_ploy <- (1 + 0.0001*X_train%*%t(X_train))^2
y_train <- train$mpg

X_test <- as.matrix(cbind(b0 = c(1), test[,c("disp","drat","wt","qsec")]))
K_test_ploy <- (1 + 0.0001*X_test%*%t(X_train))^2
y_test <- test$mpg

lam <- 0.0001
b_ploy <- solve(K_train_ploy+diag(ncol(K_train_ploy))*lam)%*%y_train
y_pred_ploy <- K_test_ploy%*%b_ploy
sum((y_test-y_pred_ploy)^2)/length(y_test)

# Gaussian kernel Radial Basis Function (RBF)
X_train <- as.matrix(cbind(b0 = c(1), train[,c("disp","drat","wt","qsec")]))
avg <- apply(X_train, 2, mean)
K_train_radial <- exp(-0.00001*(X_train-avg)%*%t(X_train-avg))
y_train <- train$mpg

X_test <- as.matrix(cbind(b0 = c(1), test[,c("disp","drat","wt","qsec")]))
K_test_radial <- exp(-0.00001*(X_test-avg)%*%t(X_train-avg))
y_test <- test$mpg

lam <- 10
b_radial <- solve(K_train_radial+diag(ncol(K_train_radial))*lam)%*%y_train
y_pred_radial <- K_test_radial%*%b_radial
sum((y_test-y_pred_radial)^2)/length(y_test)

X <- matrix(data = c(3,3,4,3,1,1), nrow = 2, ncol = 3, byrow = F)
K_ploy <- (1 + 0.0001*t(X)%*%X)^2
y <- c(1,1,-1)
a <- matrix(data = c(0.3,0.2,0.4))

(  t(a*y)%*%K_ploy%*%(a*y)  )/2 - sum(a)
