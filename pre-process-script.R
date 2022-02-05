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

### the following lines estimate the power law exponents for the number of sales/purchases per user

## power law for the number of flips a user has
sales_list = c(table(df$seller_name))
purchase_list = c(table(df$buyer_name))
s_pl = displ$new(sales_list)
est_s = estimate_xmin(s_pl)
s_pl$setXmin(est_s)
fit.data <- lines(s_pl, draw = F)
plot.data <- plot(s_pl, draw = F)
## second power law after 1800
power.law.fit(sales_list,1800)
s_pl2 = s_pl
s_pl2$xmin = 1800
s_pl2$pars = power.law.fit(sales_list,1800)$alpha
fit.data2 <- lines(s_pl2, draw=F)
plot.data2 <- plot(s_pl, draw=F)
pl1<-ggplot(plot.data) + geom_point(aes(x=log(x), y=log(y))) + labs(x="log(# of sales)", y="log(CDF)", title="Sales per user") + theme_bw(base_size=17) + annotate("text", x=5, y=0.5, label=paste0("Exponents: (",as.character(round(2.4,1)),",",as.character(round(3.4,1)),")")) +geom_line(data=fit.data, aes(x=log(x), y=log(y)),size=1.1, colour="red") + geom_line(data=fit.data2, aes(x=log(x), y=log(y)), size=1.1,colour = "blue")

p_pl = displ$new(purchase_list)
est_p = estimate_xmin(p_pl)
p_pl$setXmin(est_p)
fit.data <- lines(p_pl, draw = F)
plot.data <- plot(p_pl, draw = F)
## second power law after 1800
power.law.fit(sales_list,1800)
p_pl2 = p_pl
p_pl2$xmin = 2000
p_pl2$pars = power.law.fit(sales_list,2000)$alpha
fit.data2 <- lines(p_pl2, draw=F)
plot.data2 <- plot(p_pl, draw=F)
pl2<-ggplot(plot.data) + geom_point(aes(x=log(x), y=log(y))) + labs(x="log(# of purchases)", y="log(CDF)", title="Purchases per user") + theme_bw(base_size=17) + annotate("text", x=5, y=0.5, label=paste0("Exponents: (",as.character(round(2.6,1)),",",as.character(round(2.9,1)),")")) +geom_line(data=fit.data, aes(x=log(x), y=log(y)),size=1.1, colour="red") + geom_line(data=fit.data2, aes(x=log(x), y=log(y)), size=1.1,colour = "blue")


