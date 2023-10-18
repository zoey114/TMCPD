library(igraph)
library(sna)
library(reshape2)
library(HiveR)
library(SDMTools)
library(RColorBrewer)
library(plyr)
library(ggplot2)
library(GGally)
library(grid)
library(gridExtra)
library(reshape2)
library(igraph)
library(dplyr)
library(countrycode)
library(EnvCpt)
rm(list=ls(all=TRUE))
setwd("/Users/zhenwang/Documents/new_IR/")

######################################################
yr = seq(1990,2010) 
load("./data/nonzero.dca.RData")
#### remove year 1,2,3 
res4_21 = mod_cluster(Adj = nonzero.dca, yr, start=4)
setwd("/Users/zhenwang/Documents/new_IR/")
save(res4_21, file = "./data/mod4-21.RData")

#### remove year 1-4
res5_21 = mod_cluster(Adj = nonzero.dca, yr, start=5)
setwd("/Users/zhenwang/Documents/new_IR/")
save(res5_21, file = "./data/mod5-21.RData")

#### remove year 1-5
res6_21 = mod_cluster(Adj = nonzero.dca, yr, start=6)
setwd("/Users/zhenwang/Documents/new_IR/")
save(res6_21, file = "./data/mod6-21.RData")


#### remove year 18,19,20,21
yr_1 = seq(1990, 2006)
res1_17 = mod_cluster(Adj = nonzero.dca, yr_1, start=1)
setwd("/Users/zhenwang/Documents/new_IR/")
save(res1_17, file = "./data/mod1-17.RData")

#### remove year 19,20,21
yr_1 = seq(1990, 2007)
res1_18 = mod_cluster(Adj = nonzero.dca, yr_1, start=1)
setwd("/Users/zhenwang/Documents/new_IR/")
save(res1_18, file = "./data/mod1-18.RData")


##### compare with change point ####
res = res4_21
mod = res$modularity
adj = res$adj
cl = res$membership
plot(c(1990:2010),mod,xlab="year",ylab="modularity")
# change point location
library(EnvCpt)
mod = na.omit(mod)
fit_envcpt = envcpt(mod) # Fit all models at once
fit_envcpt$summary  # Show log-likelihoods
plot(fit_envcpt)
fit_envcpt = envcpt(mod, models="meancpt") 
plot(fit_envcpt)
cp.location = fit_envcpt$meancpt@cpts 
cp.location # [1]  6 13 18
# 9,16,21
# # first: start at 10. 
# res10_21 = s3 #s3 = mod_cluster(Adj = nonzero.dca, yr, start=10)
# mod = na.omit(res10_21$modularity)
# fit_envcpt = envcpt(mod) # Fit all models at once
# fit_envcpt$summary  # Show log-likelihoods
# plot(fit_envcpt)
# fit_envcpt = envcpt(mod, models="meancpt") 
# plot(fit_envcpt)
# cp.location = fit_envcpt$meancpt@cpts 
# cp.location #[1]  7 12
# # second: start at 17
# res17_21 = mod_cluster(Adj = nonzero.dca, yr, start=17)
# setwd("/Users/zhenwang/Documents/new_IR/")
# save(res17_21, "./data/res17_21.RData")


res = res5_21
mod = res$modularity
adj = res$adj
cl = res$membership
plot(c(1990:2010),mod,xlab="year",ylab="modularity")
# change point location
library(EnvCpt)
mod = na.omit(mod)
fit_envcpt = envcpt(mod) # Fit all models at once
fit_envcpt$summary  # Show log-likelihoods
plot(fit_envcpt)
fit_envcpt = envcpt(mod, models="meancpt") 
plot(fit_envcpt)
cp.location = fit_envcpt$meancpt@cpts 
cp.location # [1]  5 11 17
# 9,15,21

res = res6_21
mod = res$modularity
adj = res$adj
cl = res$membership
plot(c(1990:2010),mod,xlab="year",ylab="modularity")
# change point location
library(EnvCpt)
mod = na.omit(mod)
fit_envcpt = envcpt(mod) # Fit all models at once
fit_envcpt$summary  # Show log-likelihoods
plot(fit_envcpt)
fit_envcpt = envcpt(mod, models="meancpt") 
plot(fit_envcpt)
cp.location = fit_envcpt$meancpt@cpts 
cp.location # [1] 5 10 16
# 10,15,21

res = res1_17
mod = res$modularity
adj = res$adj
cl = res$membership
plot(c(1990:2010),mod,xlab="year",ylab="modularity")
# change point location
library(EnvCpt)
mod = na.omit(mod)
fit_envcpt = envcpt(mod) # Fit all models at once
fit_envcpt$summary  # Show log-likelihoods
plot(fit_envcpt)
fit_envcpt = envcpt(mod, models="meancpt") 
plot(fit_envcpt)
cp.location = fit_envcpt$meancpt@cpts 
cp.location # [1]  5 10 17
# 5, 10, 17

res = res1_18
mod = res$modularity
adj = res$adj
cl = res$membership
plot(c(1990:2010),mod,xlab="year",ylab="modularity")
# change point location
library(EnvCpt)
mod = na.omit(mod)
fit_envcpt = envcpt(mod) # Fit all models at once
fit_envcpt$summary  # Show log-likelihoods
plot(fit_envcpt)
fit_envcpt = envcpt(mod, models="meancpt") 
plot(fit_envcpt)
cp.location = fit_envcpt$meancpt@cpts 
cp.location # [1]  5 10 18
# 5, 10, 18

#### start from the change point, and cluster ####
yr = seq(1990,2010) 
res9_21 = mod_cluster(Adj = nonzero.dca, yr, start=9)
res10_21 = s3 #mod_cluster(Adj = nonzero.dca, yr, start=10)
res15_21 = s4 #mod_cluster(Adj = nonzero.dca, yr, start=15)
res16_21 = mod_cluster(Adj = nonzero.dca, yr, start=16)
res17_21 = mod_cluster(Adj = nonzero.dca, yr, start=17)
setwd("/Users/zhenwang/Documents/new_IR/")
save(res9_21, file="./data/res9_21.RData")
##### compare with number of communities ####
adj = nonzero.dca
load("./data/cluster_results.RData")
original_mem = list(s1$membership[[1]], s1$membership[[2]],  s1$membership[[3]],s1$membership[[4]],
                    s2$membership[[5]],s2$membership[[6]],s2$membership[[7]],s2$membership[[8]],s2$membership[[9]],
                    s3$membership[[10]],s3$membership[[11]],s3$membership[[12]],s3$membership[[13]],s3$membership[[14]],
                    s4$membership[[15]],s4$membership[[16]],s4$membership[[17]],
                    s4$membership[[18]],s4$membership[[19]],s4$membership[[20]],s4$membership[[21]])
number_com0 = c()
for (i in 1:21){
  cluster = original_mem[[i]]
  g1 = graph_from_adjacency_matrix(adj[[i]], mode="undirected")
  colnames(cluster) = id
  deg = igraph::degree(g1)
  Isolated = which(igraph::degree(g1)==0)
  ref = as.data.frame(table(cluster[,-Isolated])) %>% filter('Freq' > 1) %>% arrange(desc(Freq)) %>% mutate(gr1 = c(1:length(Freq)))
  number_com0 = c(number_com0, dim(ref)[1])
}
number_com0

res = res4_21
st = c(4,9,15)
ed = c(8,14,21)
res4_21$membership[1:4]
mem = c(res4_21$membership[1:8],
           res9_21$membership[9:14], 
           res15_21$membership[15:21])
library(fossil)
idx = c()
for(i in 4:21){
  t = rand.index(original_mem[[i]],mem[[i]])
  idx = c(idx, t)
} 
year = seq(1993,2010)
plot(year, idx,ylim=c(0,1),xlab="year", ylab="Rand Index",type = "o", lwd = 3, col="blue")

for (i in 1:length(st)){
  cluster = mem[[ed[i]]]
  g1 = graph_from_adjacency_matrix(adj[[ed[i]]], mode="undirected")
  colnames(cluster) = id
  deg = igraph::degree(g1)
  Isolated = which(igraph::degree(g1)==0)
  ref = as.data.frame(table(cluster[,-Isolated])) %>% filter('Freq' > 1) %>% arrange(desc(Freq)) %>% mutate(gr1 = c(1:length(Freq)))
  gr = cluster
  for(j in 1:ncol(cluster)){
    if (gr[1,j] %in% ref[,1]){
      gr[1,j] = ref[which(ref[,1]==gr[1,j]),3]
    }
    else{gr[1,j] = 0}
  }
  cl[[i]] = gr[,-Isolated]
  print(table(cl[[i]]))
}
# 1  2  3  4  5 
# 28 21 17 15 14 
# 
# 1  2  3  4  5 
# 32 27 26 17 11 
# 
# 1  2  3  4  5  6  7 
# 30 30 27 16 15 10  6 
number_com= c()
for (i in 4:21){
  cluster = mem[[i]]
  g1 = graph_from_adjacency_matrix(adj[[i]], mode="undirected")
  colnames(cluster) = id
  deg = igraph::degree(g1)
  Isolated = which(igraph::degree(g1)==0)
  ref = as.data.frame(table(cluster[,-Isolated])) %>% filter('Freq' > 1) %>% arrange(desc(Freq)) %>% mutate(gr1 = c(1:length(Freq)))
  number_com = c(number_com, dim(ref)[1])
}
number_com 
diff = number_com - number_com0[4:21]
year = seq(1993,2010)
plot(year, diff, xlab = "Year", ylab="After - Before", ylim=c(-3,3),type = "o", lwd = 3, col="blue")
title(main="Difference of number of communites", )
abline(h = 0, col = "blue", lty = 3, lwd=3)


res = res5_21
st = c(5,9,15)
ed = c(8,14,21)
mem = c(res5_21$membership[1:8],
        res9_21$membership[9:14], 
        res15_21$membership[15:21])
library(fossil)
idx = c()
for(i in 5:21){
  t = rand.index(original_mem[[i]],mem[[i]])
  idx = c(idx, t)
} 
year = seq(1994,2010)
plot(year, idx,ylim=c(0,1),xlab="year", ylab="Rand Index",type = "o", lwd = 3, col="blue")

for (i in 1:length(st)){
  cluster = mem[[ed[i]]]
  g1 = graph_from_adjacency_matrix(adj[[ed[i]]], mode="undirected")
  colnames(cluster) = id
  deg = igraph::degree(g1)
  Isolated = which(igraph::degree(g1)==0)
  ref = as.data.frame(table(cluster[,-Isolated])) %>% filter('Freq' > 1) %>% arrange(desc(Freq)) %>% mutate(gr1 = c(1:length(Freq)))
  gr = cluster
  for(j in 1:ncol(cluster)){
    if (gr[1,j] %in% ref[,1]){
      gr[1,j] = ref[which(ref[,1]==gr[1,j]),3]
    }
    else{gr[1,j] = 0}
  }
  cl[[i]] = gr[,-Isolated]
  print(table(cl[[i]]))
}
# 1  2  3  4  5  6 
# 26 19 17 14 13  6 
# 
# 1  2  3  4  5 
# 32 27 26 17 11 
# 
# 1  2  3  4  5  6  7 
# 30 30 27 16 15 10  6 
number_com= c()
for (i in 5:21){
  cluster = mem[[i]]
  g1 = graph_from_adjacency_matrix(adj[[i]], mode="undirected")
  colnames(cluster) = id
  deg = igraph::degree(g1)
  Isolated = which(igraph::degree(g1)==0)
  ref = as.data.frame(table(cluster[,-Isolated])) %>% filter('Freq' > 1) %>% arrange(desc(Freq)) %>% mutate(gr1 = c(1:length(Freq)))
  number_com = c(number_com, dim(ref)[1])
}
number_com 
diff = number_com - number_com0[5:21]
year = seq(1994,2010)
plot(year, diff, xlab = "Year", ylab="After - Before", ylim=c(-3,3),type = "o", lwd = 3, col="blue")
title(main="Difference of number of communites", )
abline(h = 0, col = "blue", lty = 3, lwd=3)


res = res6_21
st = c(6,10,15)
ed = c(9,14,21)
mem = c(res6_21$membership[1:9],
        res10_21$membership[10:14], 
        res15_21$membership[15:21])
library(fossil)
idx = c()
for(i in 6:21){
  t = rand.index(original_mem[[i]],mem[[i]])
  idx = c(idx, t)
} 
year = seq(1995,2010)
plot(year, idx,ylim=c(0,1),xlab="year", ylab="Rand Index",type = "o", lwd = 3, col="blue")


for (i in 1:length(st)){
  cluster = mem[[ed[i]]]
  g1 = graph_from_adjacency_matrix(adj[[ed[i]]], mode="undirected")
  colnames(cluster) = id
  deg = igraph::degree(g1)
  Isolated = which(igraph::degree(g1)==0)
  ref = as.data.frame(table(cluster[,-Isolated])) %>% filter('Freq' > 1) %>% arrange(desc(Freq)) %>% mutate(gr1 = c(1:length(Freq)))
  gr = cluster
  for(j in 1:ncol(cluster)){
    if (gr[1,j] %in% ref[,1]){
      gr[1,j] = ref[which(ref[,1]==gr[1,j]),3]
    }
    else{gr[1,j] = 0}
  }
  cl[[i]] = gr[,-Isolated]
  print(table(cl[[i]]))
}
# 1  2  3  4  5  6 
# 26 19 17 14 13  6 
# 
# 1  2  3  4  5 
# 30 27 27 18 11 
# 
# 1  2  3  4  5  6  7 
# 30 30 27 16 15 10  6 
number_com= c()
for (i in 6:21){
  cluster = mem[[i]]
  g1 = graph_from_adjacency_matrix(adj[[i]], mode="undirected")
  colnames(cluster) = id
  deg = igraph::degree(g1)
  Isolated = which(igraph::degree(g1)==0)
  ref = as.data.frame(table(cluster[,-Isolated])) %>% filter('Freq' > 1) %>% arrange(desc(Freq)) %>% mutate(gr1 = c(1:length(Freq)))
  number_com = c(number_com, dim(ref)[1])
}
number_com 
diff = number_com - number_com0[6:21]
year = seq(1995,2010)
plot(year, diff, xlab = "Year", ylab="After - Before", ylim=c(-3,3),type = "o", lwd = 3, col="blue")
title(main="Difference of number of communites", )
abline(h = 0, col = "blue", lty = 3, lwd=3)


res = res1_17
st = c(1,5,10)
ed = c(4,9,17)
mem = c(res1_17$membership[1:4],
        res5_21$membership[5:9], 
        res10_21$membership[10:17])
library(fossil)
idx = c()
for(i in 1:17){
  t = rand.index(original_mem[[i]],mem[[i]])
  idx = c(idx, t)
} 
year = seq(1990,2006)
plot(year, idx,ylim=c(0,1),xlab="year", ylab="Rand Index",type = "o", lwd = 3, col="blue")

for (i in 1:length(st)){
  cluster = mem[[ed[i]]]
  g1 = graph_from_adjacency_matrix(adj[[ed[i]]], mode="undirected")
  colnames(cluster) = id
  deg = igraph::degree(g1)
  Isolated = which(igraph::degree(g1)==0)
  ref = as.data.frame(table(cluster[,-Isolated])) %>% filter('Freq' > 1) %>% arrange(desc(Freq)) %>% mutate(gr1 = c(1:length(Freq)))
  gr = cluster
  for(j in 1:ncol(cluster)){
    if (gr[1,j] %in% ref[,1]){
      gr[1,j] = ref[which(ref[,1]==gr[1,j]),3]
    }
    else{gr[1,j] = 0}
  }
  cl[[i]] = gr[,-Isolated]
  print(table(cl[[i]]))
}
# 1  2  3  4  5  6  7  8 
# 17 12 11  9  8  6  4  3 
# 
# 1  2  3  4  5 
# 30 21 20 20 12 
# 
# 1  2  3  4  5  6  7 
# 29 26 20 18 18 11  2 
number_com= c()
for (i in 1:17){
  cluster = mem[[i]]
  g1 = graph_from_adjacency_matrix(adj[[i]], mode="undirected")
  colnames(cluster) = id
  deg = igraph::degree(g1)
  Isolated = which(igraph::degree(g1)==0)
  ref = as.data.frame(table(cluster[,-Isolated])) %>% filter('Freq' > 1) %>% arrange(desc(Freq)) %>% mutate(gr1 = c(1:length(Freq)))
  number_com = c(number_com, dim(ref)[1])
}
number_com 
diff = number_com - number_com0[1:17]
year = seq(1990,2006)
plot(year, diff, xlab = "Year", ylab="After - Before", ylim=c(-3,3),type = "o", lwd = 3, col="blue")
title(main="Difference of number of communites", )
abline(h = 0, col = "blue", lty = 3, lwd=3)


res = res1_18
st = c(1,5,10)
ed = c(4,9,18)
mem = c(res1_18$membership[1:4],
        res5_21$membership[5:9], 
        res10_21$membership[10:18])
library(fossil)
idx = c()
for(i in 1:18){
  t = rand.index(original_mem[[i]],mem[[i]])
  idx = c(idx, t)
} 
year = seq(1990,2007)
plot(year, idx,ylim=c(0,1),xlab="year", ylab="Rand Index",type = "o", lwd = 3, col="blue")

for (i in 1:length(st)){
  cluster = mem[[ed[i]]]
  g1 = graph_from_adjacency_matrix(adj[[ed[i]]], mode="undirected")
  colnames(cluster) = id
  deg = igraph::degree(g1)
  Isolated = which(igraph::degree(g1)==0)
  ref = as.data.frame(table(cluster[,-Isolated])) %>% filter('Freq' > 1) %>% arrange(desc(Freq)) %>% mutate(gr1 = c(1:length(Freq)))
  gr = cluster
  for(j in 1:ncol(cluster)){
    if (gr[1,j] %in% ref[,1]){
      gr[1,j] = ref[which(ref[,1]==gr[1,j]),3]
    }
    else{gr[1,j] = 0}
  }
  cl[[i]] = gr[,-Isolated]
  print(table(cl[[i]]))
}
# 1  2  3  4  5  6  7  8 
# 17 12 11  9  8  6  4  3 
# 
# 1  2  3  4  5 
# 30 21 20 20 12 
# 
# 1  2  3  4  5  6 
# 40 39 15 15 10  2 
number_com= c()
for (i in 1:18){
  cluster = mem[[i]]
  g1 = graph_from_adjacency_matrix(adj[[i]], mode="undirected")
  colnames(cluster) = id
  deg = igraph::degree(g1)
  Isolated = which(igraph::degree(g1)==0)
  ref = as.data.frame(table(cluster[,-Isolated])) %>% filter('Freq' > 1) %>% arrange(desc(Freq)) %>% mutate(gr1 = c(1:length(Freq)))
  number_com = c(number_com, dim(ref)[1])
}
number_com 
diff = number_com - number_com0[1:18]
year = seq(1990,2007)
plot(year, diff, xlab = "Year", ylab="After - Before", ylim=c(-3,3),type = "o", lwd = 3, col="blue")
title(main="Difference of number of communites", )
abline(h = 0, col = "blue", lty = 3, lwd=3)

######### remove nodes ###############
avg_degrees <- numeric()
# Calculate degree for each node in each network
for (i in 1:length(nonzero.dca)) {
  network <- graph.adjacency(nonzero.dca[[i]], mode = "undirected")
  degrees <- igraph::degree(network)
  if (i == 1) {
    avg_degrees <- degrees
  } else {
    avg_degrees <- avg_degrees + degrees
  }
}
avg_degrees <- avg_degrees / length(nonzero.dca)
# Create a data frame with node IDs and their average degrees
node_data <- data.frame(Node = 1:length(avg_degrees), Avg_Degree = avg_degrees)
# Sort nodes by average degree
sorted_nodes <- node_data[order(node_data$Avg_Degree), ]

# remove the lowest 5 nodes 
rmv.idx = as.numeric(sorted_nodes[1:5,1])
rmv = as.numeric(rownames(sorted_nodes[1:5,])) #[1]  42  95 520 581 781
countrycode(rmv, "cown", "country.name") 
# [1] "Dominican Republic" "Panama"             "Somalia"           
# [4] "Comoros"            "Maldives"
adj.rm5 = lapply(nonzero.dca, function(df){df[-rmv.idx, -rmv.idx]})
res_rm5 = mod_cluster(Adj = adj.rm5, yr, start=1)
setwd("/Users/zhenwang/Documents/new_IR/")
save(res_rm5, file = "./data/mod.rm5.RData")

load("./data/mod.rm5.RData")
res = res_rm5 
mod = res$modularity
adj = res$adj
cl = res$membership
plot(c(1990:2010),mod,xlab="year",ylab="modularity")
# change point location
library(EnvCpt)
mod = na.omit(mod)
fit_envcpt = envcpt(mod) # Fit all models at once
fit_envcpt$summary  # Show log-likelihoods
plot(fit_envcpt)
fit_envcpt = envcpt(mod, models="meancpt") 
plot(fit_envcpt)
cp.location = fit_envcpt$meancpt@cpts 
cp.location # [1]  5 10 15 21
###
res_rm5_5 = mod_cluster(Adj = adj.rm5, yr, start=5)
res_rm5_10 = mod_cluster(Adj = adj.rm5, yr, start=10)
original_mem = list(s1$membership[[1]], s1$membership[[2]],  s1$membership[[3]],s1$membership[[4]],
                    s2$membership[[5]],s2$membership[[6]],s2$membership[[7]],s2$membership[[8]],s2$membership[[9]],
                    s3$membership[[10]],s3$membership[[11]],s3$membership[[12]],s3$membership[[13]],s3$membership[[14]],
                    s4$membership[[15]],s4$membership[[16]],s4$membership[[17]],
                    s4$membership[[18]],s4$membership[[19]],s4$membership[[20]],s4$membership[[21]])

st = c(1,5,10)
ed = c(4,9,21)
mem = c(res_rm5$membership[1:4],
        res_rm5_5$membership[5:9], 
        res_rm5_10$membership[10:21])
library(fossil)
idx = c()
for(i in 1:21){
  t = rand.index(original_mem[[i]][-rmv.idx],mem[[i]])
  idx = c(idx, t)
} 
year = seq(1990,2010)
plot(year, idx,ylim=c(0,1),xlab="year", ylab="Rand Index",type = "o", lwd = 3, col="blue")

for (i in 1:length(st)){
  cluster = mem[[ed[i]]]
  g1 = graph_from_adjacency_matrix(adj[[ed[i]]], mode="undirected")
  colnames(cluster) = id[-rmv.idx]
  deg = igraph::degree(g1)
  Isolated = which(igraph::degree(g1)==0)
  ref = as.data.frame(table(cluster[,-Isolated])) %>% filter('Freq' > 1) %>% arrange(desc(Freq)) %>% mutate(gr1 = c(1:length(Freq)))
  gr = cluster
  for(j in 1:ncol(cluster)){
    if (gr[1,j] %in% ref[,1]){
      gr[1,j] = ref[which(ref[,1]==gr[1,j]),3]
    }
    else{gr[1,j] = 0}
  }
  cl[[i]] = gr[,-Isolated]
  print(table(cl[[i]]))
}
# 1  2  3  4  5  6  7 
# 18 16 11  8  8  6  3 
# 
# 1  2  3  4  5 
# 30 21 21 19 12 
# 
# 1  2  3  4  5 
# 36 35 22 19 17 
number_com= c()
for (i in 1:21){
  cluster = mem[[i]]
  g1 = graph_from_adjacency_matrix(adj.rm5[[i]], mode="undirected")
  colnames(cluster) = id[-rmv.idx]
  deg = igraph::degree(g1)
  Isolated = which(igraph::degree(g1)==0)
  ref = as.data.frame(table(cluster[,-Isolated])) %>% filter('Freq' > 1) %>% arrange(desc(Freq)) %>% mutate(gr1 = c(1:length(Freq)))
  number_com = c(number_com, dim(ref)[1])
}
number_com 
diff = number_com - number_com0
year = seq(1990,2010)
plot(year, diff, xlab = "Year", ylab="After - Before", ylim=c(-3,3),type = "o", lwd = 3, col="blue")
title(main="Difference of number of communites", )
abline(h = 0, col = "blue", lty = 3, lwd=3)




########## remove the lowest 10 nodes 
rmv.idx = as.numeric(sorted_nodes[1:10,1])
rmv = as.numeric(rownames(sorted_nodes[1:10,])) #[1]  42  95 520 581 781
countrycode(rmv, "cown", "country.name") 
# [1] "Dominican Republic" "Panama"             "Somalia"           
# [4] "Comoros"            "Maldives"           "Guyana"            
# [7] "Ireland"            "Malawi"             "Liberia"           
# [10] "Sierra Leone" 
adj.rm10 = lapply(nonzero.dca, function(df){df[-rmv.idx, -rmv.idx]})
res_rm10 = mod_cluster(Adj = adj.rm10, yr, start=1)
setwd("/Users/zhenwang/Documents/new_IR/")
save(res_rm10, file = "./data/mod.rm10.RData")

res = res_rm10 
mod = res$modularity
adj = res$adj
cl = res$membership
plot(c(1990:2010),mod,xlab="year",ylab="modularity")
# change point location
library(EnvCpt)
mod = na.omit(mod)
fit_envcpt = envcpt(mod) # Fit all models at once
fit_envcpt$summary  # Show log-likelihoods
plot(fit_envcpt)
fit_envcpt = envcpt(mod, models="meancpt") 
plot(fit_envcpt)
cp.location = fit_envcpt$meancpt@cpts 
cp.location # [1] 7 16 21
###
res_rm10_7 = mod_cluster(Adj = adj.rm10, yr, start=7)
res_rm10_16 = mod_cluster(Adj = adj.rm10, yr, start=16)
original_mem = list(s1$membership[[1]], s1$membership[[2]],  s1$membership[[3]],s1$membership[[4]],
                    s2$membership[[5]],s2$membership[[6]],s2$membership[[7]],s2$membership[[8]],s2$membership[[9]],
                    s3$membership[[10]],s3$membership[[11]],s3$membership[[12]],s3$membership[[13]],s3$membership[[14]],
                    s4$membership[[15]],s4$membership[[16]],s4$membership[[17]],
                    s4$membership[[18]],s4$membership[[19]],s4$membership[[20]],s4$membership[[21]])

st = c(1,7,16)
ed = c(6,15,21)
mem = c(res_rm10$membership[1:6],
        res_rm10_7$membership[7:15], 
        res_rm10_16$membership[16:21])
library(fossil)
idx = c()
for(i in 1:21){
  t = rand.index(original_mem[[i]][-rmv.idx],mem[[i]])
  idx = c(idx, t)
} 
year = seq(1990,2010)
plot(year, idx,ylim=c(0,1),xlab="year", ylab="Rand Index",type = "o", lwd = 3, col="blue")
id = rownames(nonzero.dca[[1]])
for (i in 1:length(st)){
  cluster = mem[[ed[i]]]
  g1 = graph_from_adjacency_matrix(adj.rm10[[ed[i]]], mode="undirected")
  colnames(cluster) = id[-rmv.idx]
  deg = igraph::degree(g1)
  Isolated = which(igraph::degree(g1)==0)
  ref = as.data.frame(table(cluster[,-Isolated])) %>% filter('Freq' > 1) %>% arrange(desc(Freq)) %>% mutate(gr1 = c(1:length(Freq)))
  gr = cluster
  for(j in 1:ncol(cluster)){
    if (gr[1,j] %in% ref[,1]){
      gr[1,j] = ref[which(ref[,1]==gr[1,j]),3]
    }
    else{gr[1,j] = 0}
  }
  cl[[i]] = gr[,-Isolated]
  print(table(cl[[i]]))
}
# 1  2  3  4  5  6 
# 21 18 16 15 10  5 
# 
# 1  2  3  4 
# 38 33 32 13 
# 
# 1  2  3  4  5  6  7 
# 28 26 26 17 13  8  7
number_com= c()
for (i in 1:21){
  cluster = mem[[i]]
  g1 = graph_from_adjacency_matrix(adj.rm10[[i]], mode="undirected")
  colnames(cluster) = id[-rmv.idx]
  deg = igraph::degree(g1)
  Isolated = which(igraph::degree(g1)==0)
  ref = as.data.frame(table(cluster[,-Isolated])) %>% filter('Freq' > 1) %>% arrange(desc(Freq)) %>% mutate(gr1 = c(1:length(Freq)))
  number_com = c(number_com, dim(ref)[1])
}
number_com 
diff = number_com - number_com0
year = seq(1990,2010)
plot(year, diff, xlab = "Year", ylab="After - Before", ylim=c(-3,3),type = "o", lwd = 3, col="blue")
title(main="Difference of number of communites", )
abline(h = 0, col = "blue", lty = 3, lwd=3)



####### remove the lowest 15 nodes 
rmv.idx = as.numeric(sorted_nodes[1:15,1])
rmv = as.numeric(rownames(sorted_nodes[1:15,])) #[1]  42  95 520 581 781
countrycode(rmv, "cown", "country.name") 
# [1] "Dominican Republic" "Panama"             "Somalia"           
# [4] "Comoros"            "Maldives"           "Guyana"            
# [7] "Ireland"            "Malawi"             "Liberia"           
# [10] "Sierra Leone"       "Djibouti"           "Eritrea"           
# [13] "Lesotho"            "Botswana"           "Afghanistan"    
adj.rm15 = lapply(nonzero.dca, function(df){df[-rmv, -rmv]})
res_rm15 = mod_cluster(Adj = adj.rm15, yr, start=1)
setwd("/Users/zhenwang/Documents/new_IR/")
save(res_rm15, file = "./data/mod.rm15.RData")

mod = na.omit(res_rm10_7$modularity)
fit_envcpt = envcpt(mod) # Fit all models at once
fit_envcpt$summary  # Show log-likelihoods
plot(fit_envcpt)
fit_envcpt = envcpt(mod, models="meancpt") 
plot(fit_envcpt)
cp.location = fit_envcpt$meancpt@cpts 
cp.location #[1]  9 15


##### change point detection 


























