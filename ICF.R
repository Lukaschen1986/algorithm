library(RMySQL);  library(plyr);  library(dplyr);  library(ggplot2);  library(reshape); library(reshape2)
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
# table(order_item_2$member_id)

# ICF
ui_matrix_icf <- cast(order_item_2, member_id ~ bn, value = "nums", fill = 0) # 构建会员（行）-商品（列）关系矩阵
row.names(ui_matrix_icf) <- ui_matrix_icf[,1]
ui_matrix_icf <- ui_matrix_icf[,-1]

sim_cor <- cor(ui_matrix_icf) # 基于会员购买行为计算商品相关系数
dist_col <- -log(sim_cor/2 + 0.5) # 转换为商品距离矩阵（0-1）,越趋向于0距离越近

colnames(dist_col) <- colnames(ui_matrix_icf)
rownames(dist_col) <- colnames(ui_matrix_icf)

ICF_recommend <- function(my_df, user_id, k_num, l_num){
          item_id <- unique(subset(my_df, member_id == user_id)$bn)
          
          item_id_detail <- c()
          df_ICF <- c()
          
          for(i in 1:length(item_id)){
                    item_id_detail <- item_id[i]
                    item_idx <- which(rownames(dist_col) == as.character(item_id_detail)) # 找到目标商品在距离矩阵中的行号
                    
                    k <- k_num; l <- l_num # 0.43代表相关系数为0.3
                    knn_idx <- order(dist_col[item_idx,])[2:(k+1)] # 找到与目标商品相似的k个商品，排序，返回列号
                    
                    k_df <- data.frame(dist = dist_col[item_idx, knn_idx]) # 与目标商品相似的k个商品及其距离
                    
                    knn_item <- row.names(subset(k_df, dist <= l)) # 加过滤条件l，得到相似商品itemid
                    if(length(knn_item) == 0){knn_item <- NA}
                    
                    res <- data.frame(user_id, knn_item, end_date)
                    df_ICF <- rbind(df_ICF, res)
                    
                    if(sum(df_ICF$knn_item %in% item_id) == 0){
                              df_ICF <- df_ICF
                    }else{
                              df_ICF <- df_ICF[-which(df_ICF$knn_item %in% item_id), ]
                    }
          }
          df_ICF <- data.frame(user_id = user_id,
                               knn_item = unique(df_ICF$knn_item),
                               end_date = end_date)
          return(df_ICF)
}

user_list <- unique(order_item_2$member_id)
res <- c()

for(i in 1:length(user_list)){
          a <- ICF_recommend(my_df = order_item_2, user_id = user_list[i], k_num = 5, l_num = 0.65)
          res <- rbind(res, a)
}
