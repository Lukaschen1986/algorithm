df$am2 <- ifelse(df$am == 0, -1, 1)
y <- df$am2
X <- as.matrix(cbind(b0 = c(1), df[,1:5]))

vctr_idnt <- function(x){x/sqrt(sum(x^2))}

n <- 2000; p <- ncol(X)
w <- matrix(data = rep(0,n*p), nrow = n, ncol = p, byrow = T) # 1.set w0
e_in <- c(1); yita <- 0.00001

for(i in 2:nrow(w)){
          index <- sample(1:nrow(X),1)
          w[i,] <- w[i-1,] - yita*t(vctr_idnt(t(1/(1+exp(y*X[index,]%*%w[i-1,])))%*%(-y*X[index,])))
          e_in[i] <- sum(log(1+exp(-y*X%*%w[i,])))/nrow(X)
          if(round(e_in[i],7) == round(e_in[i-1],7)){break}
}
plot(e_in, type = "b")
w_hat <- w[which.min(e_in),] # 5.get the last w
p_hat <- 1/(1+exp(-X%*%w_hat)) # 6.predict
y_hat <- ifelse(p_hat >= 0.5, 1, -1)

sum(y_hat == y)/length(y)
