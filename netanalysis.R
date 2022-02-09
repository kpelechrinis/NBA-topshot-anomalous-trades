library(ggplot2)
library(DescTools)
library(data.table)
library(poweRlaw)
library(tidyverse)
library(sqldf)
library(igraph)
library(beeswarm)
library(igraph)

#df_resale is the data frame obtained from pre-process-script.R

#thres is the delta in the paper
thres = 1
topshot_g_min <- simplify(graph_from_data_frame(df_resale[,c("seller_name","buyer_name")], directed=TRUE))
an_net.df <- read.csv("an_net.csv")
vids_cde = c(unique(an_net.df[an_net.df$price_USD >= thres, ]$seller_name),unique(an_net.df[an_net.df$price_USD >= thres, ]$buyer_name))
g_cde_simple = simplify(induced_subgraph(topshot_g_min, vids = c(vids_cde)))

# random graphs and edge density
rand_edgeden <- rep(0,20000)

for (i in 1:20000){
	g_rand = induced_subgraph(topshot_g,vids=sample(vcount(topshot_g_min), length(V(g_cde_simple1000))))
	rand_edgeden[i] = edge_density(g_rand)
}

ggplot(data.frame(edgeden=rand_edgeden), aes(x=edgeden)) + geom_histogram(aes(y=..density..), colour="black", fill="white")+geom_density(alpha=.2, fill="#FF6666") + geom_vline(xintercept=edge_density(g_cde_simple1))

rand_pl_in <- rep(0,20000)
rand_pl_out <- rep(0,20000)
rand_pl_total <- rep(0,20000)
max_in <- rep(0,20000)
max_out <- rep(0,20000)

for (i in 1:20000){
        g_rand = induced_subgraph(topshot_g,vids=sample(vcount(topshot_g_min), length(V(g_cde_simple))))
	rand_pl_in[i] = power.law.fit(degree(g_rand,mode='in'))$alpha
	rand_pl_out[i] = power.law.fit(degree(g_rand,mode='out'))$alpha
	rand_pl_total[i] = power.law.fit(degree(g_rand,mode='total'))$alpha
	max_in[i] = max(degree(g_rand,mode="in"))
	max_out[i] = max(degree(g_rand,mode="out"))
}

rand_pl_total = rand_pl_total[which(abs(rand_pl_total) < 10)]
rand_pl_in = rand_pl_in[which(abs(rand_pl_in) < 10)]
rand_pl_out = rand_pl_out[which(abs(rand_pl_out) < 10)]



pin_1 = ggplot(data.frame(gamma=rand_pl_in),aes(x=gamma)) + geom_histogram(aes(y=..density..), colour="black", fill="white")+geom_density(alpha=.2, fill="#FF6666") + geom_vline(xintercept=power.law.fit(degree(g_cde_simple1,mode="in"))$alpha)+labs(x="In Degree Exponent", y="Density", caption = TeX("$\\delta$=1"))+ theme_bw(base_size=17)
pout_1 = ggplot(data.frame(gamma=rand_pl_out),aes(x=gamma)) + geom_histogram(aes(y=..density..), colour="black", fill="white")+geom_density(alpha=.2, fill="yellow") + geom_vline(xintercept=power.law.fit(degree(g_cde_simple1,mode="out"))$alpha)+labs(x="Out Degree Exponent", y="Density", caption = TeX("$\\delta$=1"))+ theme_bw(base_size=17)
ptot_1 = ggplot(data.frame(gamma=rand_pl_total),aes(x=gamma)) + geom_histogram(aes(y=..density..), colour="black", fill="white")+geom_density(alpha=.2, fill="green") + geom_vline(xintercept=power.law.fit(degree(g_cde_simple1,mode="total"))$alpha)+labs(x="Total Degree Exponent", y="Density", caption = TeX("$\\delta$=1"))+ theme_bw(base_size=17)

#clustering coefficient
transitivity(topshot_g_min)
transitivity(simplify(induced_subgraph(topshot_g_min, vids = c(vids_cde))))

rand_cc<- rep(0,20000)

for (i in 1:20000){
        g_rand = induced_subgraph(topshot_g_min,vids=sample(vcount(topshot_g_min), length((vids_cde))))
        rand_cc[i] = transitivity(g_rand)
}

#HITS 

pval_hubs = rep(0,20000)
pval_auth = rep(0,20000)
for (i in 1:20000){
        g_rand = induced_subgraph(topshot_g_min,vids=sample(vcount(topshot_g_min), length((vids_cde))))
        pval_hubs[i] = ks.test(hub_score(g_rand)$vector,hub_score(topshot_g_min)$vector)$p.val
        pval_auth[i] = ks.test(authority_score(g_rand)$vector,authority_score(topshot_g_min)$vector)$p.val
}
