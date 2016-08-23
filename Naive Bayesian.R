rm(list = ls())
gc()
options(stringsAsFactors = F)

age <- c(18,22,30,40,45,50,35,21,19,55,20,32,31,60)
income <- c("high","high","high","medium","low","low","low","medium","low","medium","medium","medium","high","medium")
student <- c("no","no","no","no","yes","yes","yes","no","yes","yes","yes","no","yes","no")
credit_rating <- c("fair","excellent","fair","fair","fair","excellent","excellent","fair","fair","fair","excellent","excellent","fair","excellent")
buys_computer <- c("no","no","yes","yes","yes","no","yes","no","yes","yes","yes","yes","yes","no")
mydata <- data.frame(age, income, student, credit_rating, buys_computer)

# test
idx <- sample(1:nrow(mydata), nrow(mydata)*0.5)
test <- mydata[idx, ]

# 先验概率
p_y <- function(train, y, label_y){nrow(subset(train, y == label_y))/nrow(train)}

# 条件概率
p_x_given_y <- function(train, y, label_y, test){
          n <- nrow(test); p <- ncol(train)-1
          z1 <- matrix(data = rep(0,n*p), nrow = n, ncol = p, byrow = T)
          
          for(i in 1:n){
                    for(j in 1:p){
                              if(is.numeric(train[,j]) == T){
                                        z1[i,j] <- dnorm(test[i,j], mean(subset(train, y == label_y)[,j]), sd(subset(train, y == label_y)[,j]))   
                              }else{
                                        z1[i,j] <- (nrow(subset(train, y == label_y & train[,j] == test[i,j]))+1)/(nrow(subset(train, y == label_y))+(p))
                              }
                    }
          }
          z1 <- apply(z1,1,prod)
          return(z1)
}

# 边际概率
p_x <- function(train, test){
          n <- nrow(test); p <- ncol(train)-1
          z2 <- matrix(data = rep(0,n*p), nrow = n, ncol = p, byrow = T)
          
          for(i in 1:n){
                    for(j in 1:p){
                              if(is.numeric(train[,j]) == T){
                                        z2[i,j] <- dnorm(test[i,j], mean(train[,j]), sd(train[,j]))    
                              }else{
                                        z2[i,j] <- nrow(subset(train, train[,j] == test[i,j]))/nrow(train)
                              }
                    }
          }
          z2 <- apply(z2,1,prod)
          return(z2)
}

# 汇总
p_yes <- p_x_given_y(train = mydata, y = buys_computer, label_y = "yes", test = test)*p_y(train = mydata, y = buys_computer, label_y = "yes")/p_x(train = mydata, test = test)
p_no <- p_x_given_y(train = mydata, y = buys_computer, label_y = "no", test = test)*p_y(train = mydata, y = buys_computer, label_y = "no")/p_x(train = mydata, test = test)

pred <- data.frame(cbind(p_yes,p_no))
pred$res <- ifelse(pred[,1] > pred[,2], yes = "yes", no = "no")

sum(pred$res == test$buys_computer)/nrow(test)
