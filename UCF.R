library(RMySQL); library(plyr); library(dplyr); library(ggplot2); library(reshape); library(reshape2); library(proxy)
options(stringsAsFactors = F)

# 导入数据
mossel <- dbConnect(MySQL(), user = "root", password = "chen1986", host = "127.0.0.1", db = "mossel")

# end_date <- Sys.Date()-1
end_date <- as.Date(as.character(20160502), format = "%Y%m%d")-1
begin_date <- end_date-90+1-as.POSIXlt(as.character(end_date-90))$wday

end_date <- gsub(pattern = "-", replacement = "", x = end_date)
begin_date <- gsub(pattern = "-", replacement = "", x = begin_date)

sql <- paste("select a.*, b.member_id, b.create_date from (select * from sdb_b2c_order_items) a inner join (select order_id, member_id, from_unixtime(createtime,'%Y%m%d') as create_date from sdb_b2c_orders where pay_status = '1' and from_unixtime(createtime,'%Y%m%d') between", begin_date, "and", end_date, ") b on a.order_id = b.order_id;")
order_item <- dbGetQuery(mossel, sql) # 订单主表

order_item$order_id <- as.character(order_item$order_id)

# data cleaning
agg_1 <- aggregate(order_id ~ member_id, data = order_item, length)
agg_1$order_scale <- scale(agg_1$order_id, center = T, scale = T)
agg_2 <- subset(agg_1, order_scale < 5)
order_item_2 <- subset(order_item, member_id %in% agg_2$member_id)

# UCF
ui_matrix_ucf <- cast(order_item_2, member_id ~ bn, value = "nums", fill = NA) # 构建会员（行）-商品（列）关系矩阵
row.names(ui_matrix_ucf) <- ui_matrix_ucf[,1]
ui_matrix_ucf <- ui_matrix_ucf[,-1]

# dist_row <- as.matrix(dist(ui_matrix, method = "euclidean", diag = T, upper = T))
dist_row <- as.matrix(dist(ui_matrix_ucf, method = "cosine", diag = T, upper = T))
colnames(dist_row) <- rownames(ui_matrix_ucf)
rownames(dist_row) <- rownames(ui_matrix_ucf)

UCF_recommend <- function(my_df, user_id, k_num, l_num){
          item_id <- unique(subset(my_df, member_id == user_id)$bn)
          user_idx <- which(rownames(dist_row) == as.character(user_id))
          
          k <- k_num; l <- l_num
          knn_idx <- order(dist_row[user_idx,])[2:(k+1)]
          
          k_df <- data.frame(dist = dist_row[user_idx, knn_idx])
          
          knn_user <- row.names(subset(k_df, dist <= l))
          knn_item <- unique(subset(my_df, member_id %in% knn_user)$bn)
          if(length(knn_item) == 0){knn_item <- NA}
          
          df_UCF <- data.frame(user_id, knn_item, end_date)
          
          if(sum(df_UCF$knn_item %in% item_id) == 0){
                    df_UCF <- df_UCF
          }else{
                    df_UCF <- df_UCF[-which(df_UCF$knn_item %in% item_id),]
          }
          
          df_UCF <- data.frame(user_id = user_id,
                               knn_item = unique(df_UCF$knn_item),
                               end_date = end_date)
          return(df_UCF)
}
# UCF_recommend(my_df = order_item_2, k_num = 5, l_num = 0.5, user_id = 28409)

user_list <- unique(order_item_2$member_id)
res <- c()

for(i in 1:length(user_list)){
          a <- UCF_recommend(my_df = order_item_2, user_id = user_list[i], k_num = 5, l_num = 0.5)
          res <- rbind(res, a)
}
