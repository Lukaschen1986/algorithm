rm(list = ls())
gc()

df <- mtcars
df$am2 <- ifelse(df$am == 0, -1, 1)
y <- df$am2
X <- as.matrix(cbind(b0 = c(1), scale(df[,1:3], center = T, scale = T)))
X <- as.matrix(cbind(b0 = c(1), df[,1:3]))

# PLA
n <- 10000; p <- ncol(X)
w <- matrix(data = rep(0,n*p), nrow = n, ncol = p, byrow = T)
e_in <- c(1)

for(i in 2:nrow(w)){
          w[i,] <- w[i-1,] + y[which.min(X%*%w[i-1,]*y)]*X[which.min(X%*%w[i-1,]*y),]
          e_in[i] <- sum(sign(X%*%w[i,]) != y)
          if(round(e_in[i],7) == round(e_in[i-1],7)){break}
}
# min(e_in, na.rm = T); which.min(e_in)
w_hat <- w[which.min(e_in), ]
y_hat <- sign(X%*%w_hat)
sum(y_hat == y)/length(y)

# SVM
obj <- function(w){(t(w[1:p]) %*% rbind(0, cbind(0, diag(p-1))) %*% w[1:p])/2}
dm_ui <- y*X
dm_ci <- rep(1,nrow(X))

ind <- which(dm_ui %*% w_hat >= 1)
X_2 <- X[ind,]
y_2 <- y[ind]
dm_ui_2 <- y_2*X_2
dm_ci_2 <- rep(1,nrow(X_2))

par <- constrOptim(theta = w_hat, f = obj, ui = dm_ui_2, ci = dm_ci_2, grad = NULL, control = list(maxit = 5000))$par
y_hat_svm <- sign(X %*% par)
sum(y_hat_svm == y)/length(y)
