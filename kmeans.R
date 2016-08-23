# data whiten
x <- runif(n = 30)
y <- runif(n = 30)
z <- runif(n = 30)

x_scale <- scale(x)
y_scale <- scale(y)
z_scale <- scale(z)

X <- as.matrix(cbind(x_scale, y_scale, z_scale))

D <- svd(cov(X))$d
U <- svd(cov(X))$u

U %*% diag(D) %*% t(U) # 还原协方差矩阵
md_whiten <- U %*% solve(sqrt(diag(D))) %*% t(U)
X_whiten <- X %*% md_whiten

# data_scale
df_merge_scale <- data.frame(member_id = df_merge[,1], scale(df_merge[,-1], center = F, scale = T))

# kmeans
euc_dist <- function(x,y){sqrt(sum((x-y)^2))}
df <- df_merge_scale[,-1]

dist_df <- as.matrix(dist(df, method = "euclidean", diag = T, upper = T))

set.seed(1234)
center_1_idx <- sample(1:nrow(dist_df),1)
center_2_idx <- which.max(dist_df[center_1_idx,])
center_3_idx <- which.max(apply(dist_df[c(center_1_idx,center_2_idx),], 2, mean))

center_1 <- rbind(0, df[center_1_idx,])
center_2 <- rbind(0, df[center_2_idx,])
center_3 <- rbind(0, df[center_3_idx,])

for(k in 2:50){
          comp_1 <- c(); comp_2 <- c(); comp_3 <- c()
          for(i in 1:nrow(df)){
                    comp_1[i] <- euc_dist(df[i,], center_1[k,])
                    comp_2[i] <- euc_dist(df[i,], center_2[k,])
                    comp_3[i] <- euc_dist(df[i,], center_3[k,])
          }
          df_cls <- data.frame(comp_1, comp_2, comp_3)
          df_cls$row_min <- apply(df_cls,1,min)
          df_cls_2 <- df_cls[,1:3] == df_cls[,4]
          
          label <- c()
          for(j in 1:nrow(df_cls_2)){
                    if(df_cls_2[j,1] == T){label[j] <- 1}
                    if(df_cls_2[j,2] == T){label[j] <- 2}
                    if(df_cls_2[j,3] == T){label[j] <- 3}
          }
          df_new <- data.frame(df, label)
          center_1 <- rbind(center_1, apply(subset(df_new[1:ncol(df)], label == 1), 2, mean))
          center_2 <- rbind(center_2, apply(subset(df_new[1:ncol(df)], label == 2), 2, mean))
          center_3 <- rbind(center_3, apply(subset(df_new[1:ncol(df)], label == 3), 2, mean))
          
          if(sum(center_1[k,] == center_1[k-1,]) == ncol(df) 
             & sum(center_2[k,] == center_2[k-1,]) == ncol(df) 
             & sum(center_3[k,] == center_3[k-1,]) == ncol(df)){break}
}

center_1 <- center_1[nrow(center_1),]
center_2 <- center_2[nrow(center_2),]
center_3 <- center_3[nrow(center_3),]

centers <- rbind(center_1, center_2, center_3)
centers$label <- c(1:nrow(centers))

# SSE
sse_1 <- sum((subset(df_new[1:ncol(df)], label == 1) - as.numeric(center_1))^2)/nrow(subset(df_new[1:ncol(df)], label == 1))
sse_2 <- sum((subset(df_new[1:ncol(df)], label == 2) - as.numeric(center_2))^2)/nrow(subset(df_new[1:ncol(df)], label == 2))
sse_3 <- sum((subset(df_new[1:ncol(df)], label == 3) - as.numeric(center_3))^2)/nrow(subset(df_new[1:ncol(df)], label == 3))
sse <- sse_1+sse_2+sse_3

df_origin <- data.frame(member_id = agg_f$member_id,
                        frequency = agg_f$frequency,
                        monetary = agg_m$amount,
                        unit = agg_unit_mult$unit_mult,
                        unit_bao = exp(df_merge$bao_log),
                        label = df_new$label)

df_label <- data.frame(label = c(1:nrow(centers)),
                       frequency = round(tapply(df_origin$frequency, df_origin$label, mean),0),
                       monetary = round(tapply(df_origin$monetary, df_origin$label, mean),0),
                       unit = round(tapply(df_origin$unit, df_origin$label, mean),0),
                       unit_bao = round(tapply(df_origin$unit_bao, df_origin$label, mean),0))

df_label$label_update <- c() # 1-“高价值”；2-“中等价值”；3-“低价值”
df_label[which.max(df_label$monetary), c("label_update")] <- 1
df_label[which.min(df_label$monetary), c("label_update")] <- 3
df_label$label_update[!df_label$label_update %in% c(1,3)] <- 2

df_new$label_update[df_new$label == 1] <- subset(df_label, label == 1)$label_update
df_new$label_update[df_new$label == 2] <- subset(df_label, label == 2)$label_update
df_new$label_update[df_new$label == 3] <- subset(df_label, label == 3)$label_update
df_new$member_id <- df_origin$member_id

df_origin <- cbind(df_origin, label_update = df_new$label_update)
