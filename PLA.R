rm(list = ls())
gc()

df <- mtcars
df$am2 <- ifelse(df$am == 0, -1, 1)
y <- df$am2
X <- as.matrix(cbind(b0 = c(1), df[,1:5]))

n <- 10000; p <- ncol(X)
w <- matrix(data = rep(0,n*p), nrow = n, ncol = p, byrow = T)
e_in <- c(100); yita <- 1

for(i in 2:nrow(w)){
          w[i,] <- w[i-1,] + yita*y[which.min(X%*%w[i-1,]*y)]*X[which.min(X%*%w[i-1,]*y),]
          e_in[i] <- sum(sign(X%*%w[i,]) != y)
          if(round(e_in[i],7) == round(e_in[i-1],7)){break}
}
min(e_in, na.rm = T)
which.min(e_in)
w_hat <- w[which.min(e_in), ]
y_hat <- sign(X%*%w_hat)
sum(y_hat != y)
