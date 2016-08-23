# 1.optim
y <- df[,4]
X <- as.matrix(cbind(b0 = c(1), df[,1:3]))

obj <- function(w){sum((y - X%*%c(w[1:4]))^2)}
optim(par = c(1,1,1,1), fn = obj, method = "BFGS")

# 2.func
y <- df[,4]
X <- as.matrix(cbind(b0 = c(1), df[,1:3]))

if(det(t(X)%*%X) == 0){stop("The matrix is singular, can't do inverse!")}

w <- solve(t(X)%*%X) %*% t(X) %*% y
