rm(list = ls())
gc()

df <- mtcars
df <- df[,c("mpg","disp","drat","wt","qsec","cyl")]

df$cyl_is4 <- ifelse(df$cyl == 4, 1, -1)
df$cyl_is6 <- ifelse(df$cyl == 6, 1, -1)
df$cyl_is8 <- ifelse(df$cyl == 8, 1, -1)

X <- as.matrix(cbind(b0 = c(1), df[,1:5]))
vctr_idnt <- function(x){x/sqrt(sum(x^2))}

multi_logit_reg <- function(X, y){
          if(det(t(X)%*%X) == 0){stop("The matrix is singular, can't do inverse!")}
          w_lin <- solve(t(X)%*%X) %*% t(X) %*% y
          
          n <- 50000; p <- ncol(X)
          w <- rbind(t(w_lin), matrix(data = rep(0,n*p), nrow = n, ncol = p, byrow = T))
          e_in <- c(1); yita <- 0.001
          
          for(i in 2:nrow(w)){
                    w[i,] <- w[i-1,] - yita*t(vctr_idnt(t(1/(1+exp(y*X%*%w[i-1,])))%*%(-y*X)/nrow(X))) 
                    e_in[i] <- sum(log2(1+exp(-y*X%*%w[i,])))/nrow(X) 
                    if(round(e_in[i],7) == round(e_in[i-1],7)){break}
          }
          
          w_hat <- w[which.min(e_in),]
          p_hat <- 1/(1+exp(-X%*%w_hat))
          
          return(p_hat)
}
p_hat_cyl4 <- multi_logit_reg(X = X, y = df$cyl_is4)
p_hat_cyl6 <- multi_logit_reg(X = X, y = df$cyl_is6)
p_hat_cyl8 <- multi_logit_reg(X = X, y = df$cyl_is8)

p_total <- data.frame(cyl_4 = p_hat_cyl4,
                      cyl_6 = p_hat_cyl6,
                      cyl_8 = p_hat_cyl8)

p_total$row_max <- apply(p_total,1,max)
p_total_2 <- p_total[,1:3] == p_total[,4]

y_hat <- c()
for(i in 1:nrow(p_total_2)){
          if(p_total_2[i,1] == T){y_hat[i] <- 4}
          if(p_total_2[i,2] == T){y_hat[i] <- 6}
          if(p_total_2[i,3] == T){y_hat[i] <- 8}
}

sum(y_hat == df$cyl)/length(df$cyl)
