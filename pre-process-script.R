library(ggplot2)
library(DescTools)
library(data.table)
library(poweRlaw)
library(tidyverse)
library(sqldf)
library(igraph)
library(beeswarm)

df <- read.csv('nba_topshot_transactions.csv')

t = as.numeric(df$price_USD)
ind = which(is.na(t))

df = df[-ind,]

df$Date <- as.Date(df$transaction_timestamp)
df$Date <- format(as.POSIXct(df$Date,format="%Y-%m-%d %H:%M:%S"),"%Y:%m:%d")
df$Date <- as.Date(df$Date,format = "%Y:%m:%d")


flippers <- sqldf('with pen as (select moment_unique_id,seller_id as original_seller_id, seller_name as original_seller_name,buyer_id as flipper_id, buyer_name as flipper_name, price_USD as purchase_price, lead(price_USD) over (partition by moment_unique_id order by transaction_timestamp asc) as sold_price, transaction_timestamp as purchase_ts,lead(buyer_id) over (partition by moment_unique_id order by transaction_timestamp asc) as final_buyer_id,lead(buyer_name) over (partition by moment_unique_id order by transaction_timestamp asc) as final_buyer_name,lead(transaction_timestamp) over (partition by moment_unique_id order by transaction_timestamp asc) as sold_ts, lead(price_USD) over (partition by moment_unique_id order by transaction_timestamp asc) - price_USD as profit from df order by moment_unique_id) select * from pen where sold_ts is not null')

realized_gains <- sqldf('select flipper_id, flipper_name, sum(profit) as flip_pnl, count(profit) as flip_cnt from flippers where flipper_id like \'%-%\' group by 1,2 order by 3 desc')

extra_features <- sqldf('with pen as (select transaction_id, moment_unique_id,price_USD as sale_price, lag(price_USD) over (partition by moment_unique_id order by transaction_timestamp asc) as baught_price, avg(price_USD) over (partition by moment_id order by transaction_timestamp asc ROWS 9 PRECEDING) as comp_price, transaction_timestamp as purchase_ts,ROW_NUMBER() over (partition by moment_unique_id order by transaction_timestamp asc) as trade_cnts from df order by moment_unique_id) select * from pen where sale_price is not null')

# separate the data to trades and first sales in the data 

df_sorted <- sqldf('select * from df order by moment_unique_id')
df_sorted$baught_price = extra_features$baught_price
df_sorted$trade_cnts = extra_features$trade_cnts
df_sorted$price_USD = as.numeric(as.character(df_sorted$price_USD))
df_sorted$comp_price = extra_features$comp_price
df_sorted$circulation_count = as.numeric(as.character(df_sorted$circulation_count))
df_sorted$serial_number = as.numeric(as.character(df_sorted$serial_number))
df_first = df_sorted[which(is.na(df_sorted$baught_price)),]
df_first = df_first[which(!is.na(df_first$price_USD)),]

# build a model for the "trades"
df_sorted$baught_price = as.numeric(as.character(df_sorted$baught_price))
df_sorted$trade_cnts = as.numeric(as.character(df_sorted$trade_cnts))
df_resale = df_sorted[which(!is.na(df_sorted$baught_price)),]
df_resale = df_resale[which(!is.na(df_resale$price_USD)),]
head(df_resale)

df_resale$profit = df_resale$price_USD - df_resale$baught_price
mod_resale = lm(profit~circulation_count*is_limited+serial_number+as.factor(play_category)+ as.factor(player_name)+trade_cnts + comp_price + baught_price,data=df_resale)

df_resale$comp_profit = df_resale$comp_price-df_resale$baught_price 
mod_resale = lm(profit~circulation_count*is_limited+serial_number+as.factor(play_category)+ as.factor(player_name)+trade_cnts + comp_profit*baught_price,data=df_resale)

summary(mod_resale)

df_resale$pred_profit = predict(mod_resale,df_resale)
df_resale$profit_ae = df_resale$profit - df_resale$pred_profit

write.csv(df_resale,"data_RFCDE.csv")
