# 1.glm
fit.glm <- glm(vs ~ mpg + disp + wt, data = df, family = binomial(link = "logit"))
summary(fit.glm)

# 2.func
df$am2 <- ifelse(df$am == 0, -1, 1)
y <- df$am2
X <- as.matrix(cbind(b0 = c(1), df[,1:5]))

w_lin <- solve(t(X)%*%X) %*% t(X) %*% y
vctr_idnt <- function(x){x/sqrt(sum(x^2))}

n <- 50000; p <- ncol(X)
w <- rbind(t(w_lin), matrix(data = rep(0,n*p), nrow = n, ncol = p, byrow = T))
e_in <- c(1); yita <- 0.001 # 0.01, 0.03, 0.1, 0.3, 1, 3, 10

for(i in 2:nrow(w)){
          w[i,] <- w[i-1,] - yita*t(vctr_idnt(t(1/(1+exp(y*X%*%w[i-1,])))%*%(-y*X)/nrow(X))) 
          e_in[i] <- sum(log(1+exp(-y*X%*%w[i,])))/nrow(X) 
          if(round(e_in[i],7) == round(e_in[i-1],7)){break}
}
plot(e_in, type = "b")
w_hat <- w[which.min(e_in),] 
p_hat <- 1/(1+exp(-X%*%w_hat)) 
y_hat <- ifelse(p_hat >= 0.5, 1, -1)

sum(y_hat == y)/length(y)
