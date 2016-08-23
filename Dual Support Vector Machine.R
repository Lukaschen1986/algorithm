# 1
X <- matrix(data = c(3,3,4,3,1,1), nrow = 2, ncol = 3, byrow = F)
y <- c(1,1,-1)
a <- matrix(data = c(0.3,0.2,0.4))

t(X%*%(a*y)) %*% (X%*%(a*y)) # 2.9

# 2
x1 <- matrix(data = c(3,3))
x2 <- matrix(data = c(4,3))
x3 <- matrix(data = c(1,1))

y1 <- c(1)
y2 <- c(1)
y3 <- c(-1)

a1 <- c(0.3)
a2 <- c(0.2)
a3 <- c(0.4)

a1*a1*y1*y1*t(x1)%*%x1 +
a2*a2*y2*y2*t(x2)%*%x2 +
a3*a3*y3*y3*t(x3)%*%x3 +
2*a1*a2*y1*y2*t(x1)%*%x2 +
2*a1*a3*y1*y3*t(x1)%*%x3 +
2*a2*a3*y2*y3*t(x2)%*%x3 # 2.9

rm(list = ls())
gc()

df <- mtcars
df$am2 <- ifelse(df$am == 0, -1, 1)
y <- df$am2
X <- as.matrix(df[,1:3])

eigen(t(X%*%(a*y)) %*% (X%*%(a*y)))$values >= 0 # Q必须为半正定矩阵

# obj <- function(a){(t(a) %*% ((y*X) %*% (y*t(X))) %*% a)/2 - rep(1,nrow(X))%*%a}
obj <- function(a){(  t(X%*%(a*y)) %*% (X%*%(a*y))  )/2 - sum(a)}
dm_ui <- rbind(t(y), -t(y), rep(1,nrow(X)))
dm_ci <- c(0,0,0)
constrOptim(theta = ?, f = obj, ui = dm_ui, ci = dm_ci, grad = NULL, control = list(maxit = 5000)) # ui %*% theta - ci >= 0
