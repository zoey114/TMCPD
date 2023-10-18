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
## Import the network and monadic data in two separate objects
nets.total <- read.csv("./data/01.networks", header=TRUE, row.names=1) # [1] 554258     17
colnames(nets.total)
mons.total <- read.csv("./data/02.monadic", header=TRUE, row.names=1) # [1] 3410   17
colnames(mons.total)
############## construct dca admat #################
id <- sort(unique(nets.total$ccode1))
n <- length(id)
emat <- matrix(NA, length(id), length(id), dimnames=list(id,id))
yr = seq(1990,2010)
dca = list()
for (jj in yr) {
  net <- nets.total[nets.total$year==jj,c("ccode1","ccode2","dca")]
  net <- acast(net, ccode1~ccode2, value.var="dca")
  out <- emat
  out[rownames(out) %in% rownames(net), colnames(out) %in% colnames(net)] <- net
  out[is.na(out)] <- 0
  ## Order rows/columns
  out <- out[order(id),order(id)]
  dca[[which(yr == jj)]] = out
}
print(length(dca))

######### draw edge density plot ##########
adj = dca
n = dim(adj[[1]])[1]
y = c()
for(i in 1:length(yr)){
  g =  graph_from_adjacency_matrix(adj[[i]], mode="undirected")
  y[i] = edge_density(g, loops = FALSE)*(n*(n-1)/2)
}
pdf("density.pdf",width = 8, height = 6)
plot(yr, y,type = "b",pch=16, xlab="year", ylab="number of edges", xaxt="n", 
     main="Plot of number of edges at 21 time points")
axis(1, at=seq(min(yr), max(yr), by=1), las=2)
dev.off()

###############  remove degree-0 nodes ##########
mat = c()
for(i in 1:length(yr)){
  g = graph.adjacency(dca[[i]],mode="max")
  d = igraph::degree(g)
  mat = rbind(mat, d)
}
dca.degree = colSums(mat)
rmid = which(dca.degree == 0) 
nonzero.dca = lapply(dca, function(x){x[-rmid,-rmid]})
id = as.numeric(rownames(nonzero.dca[[1]]))
adj = nonzero.dca
save(nonzero.dca, file="./data/nonzero.dca.RData")

######### based on modularity, analysis ###############
yr = seq(1990,2010)
res = mod_cluster(Adj = nonzero.dca, yr, start=1)
setwd("/Users/zhenwang/Documents/new_IR/")
save(res, file = "./data/mod1-21.RData")
mod = res$modularity
adj = res$adj
cl = res$membership
plot(c(1990:2010),mod,xlab="year",ylab="modularity")
# change point location
library(EnvCpt)
fit_envcpt = envcpt(mod)  # Fit all models at once
fit_envcpt$summary  # Show log-likelihoods
plot(fit_envcpt)
fit_envcpt = envcpt(mod, models="meancpt") 
plot(fit_envcpt)
cp.location = fit_envcpt$meancpt@cpts 
cp.location
# [1]  5 10 15 21
# first change point: start at 6. 
res6_21 = mod_cluster(Adj = nonzero.dca, yr, start=6) 
setwd("/Users/zhenwang/Documents/new_IR/")
save(res6_21, "./data/res6_21.RData")
mod = na.omit(res6_21$modularity)
fit_envcpt = envcpt(mod)  # Fit all models at once
fit_envcpt$summary  # Show log-likelihoods
plot(fit_envcpt)
fit_envcpt = envcpt(mod, models="meancpt") 
plot(fit_envcpt)
cp.location = fit_envcpt$meancpt@cpts 
cp.location #[1]  5 10 16
# second change point: start at 11
res11_21 = mod_cluster(Adj = nonzero.dca, yr, start=11)
setwd("/Users/zhenwang/Documents/new_IR/")
save(res11_21, file = "./data/res11_21.RData")
mod = na.omit(res11_21$modularity)
fit_envcpt = envcpt(mod)  # Fit all models at once
fit_envcpt$summary  # Show log-likelihoods
plot(fit_envcpt)
fit_envcpt = envcpt(mod, models="meancpt") 
plot(fit_envcpt)
cp.location = fit_envcpt$meancpt@cpts 
cp.location #[1]  6 11
# third: start at 17. 
res17_21 = mod_cluster(Adj = nonzero.dca, yr, start=17)
setwd("/Users/zhenwang/Documents/new_IR/")
save(res17_21, file = "./data/res17_21.RData")
mod = na.omit(res17_21$modularity)
fit_envcpt = envcpt(mod)  # Fit all models at once
fit_envcpt$summary  # Show log-likelihoods
plot(fit_envcpt)
fit_envcpt = envcpt(mod, models="meancpt") 
plot(fit_envcpt)
cp.location = fit_envcpt$meancpt@cpts 
cp.location

#######  find each stage membership #######
st = c(1,6,11,17)
ed = c(5,10,16,21)
# s1 = res
# s2 = mod_cluster(Adj = nonzero.dca, yr, start=5)
# s3 = mod_cluster(Adj = nonzero.dca, yr, start=10)
# s4 = mod_cluster(Adj = nonzero.dca, yr, start=15)
# setwd("/Users/zhenwang/Documents/new_IR/")
# save(s1,s2,s3,s4, file="./data/cluster_results.RData")
res_final = c(res$membership[1:5],res6_21$membership[6:10],
              res11_21$membership[11:16], res17_21$membership[17:21])

# mod1 = s1$modularity[c(1:4)]
# mod2 = s2$modularity[c(5:9)]
# mod3 = s3$modularity[c(10:14)]
# mod4 = s4$modularity[c(15:21)]
# mod = c(mod1, mod2, mod3, mod4)
# plot(x = yr, y = mod, xlab="year", ylab="modularity score")
# abline(v=yr[4], col="blue")
# abline(v=yr[9], col="blue")
# abline(v=yr[14], col="blue")
mod1 = res$modularity[c(1:5)]
mod2 = res6_21$modularity[c(6:10)]
mod3 = res11_21$modularity[c(11:16)]
mod4 = res17_21$modularity[c(17:21)]
mod = c(mod1, mod2, mod3, mod4)
plot(x = yr, y = mod, xlab="year", ylab="modularity score")
abline(v=yr[5], col="blue")
abline(v=yr[10], col="blue")
abline(v=yr[16], col="blue")

cl = list()
st = c(1,6,11,17)
ed = c(5,10,16,21)
adj = nonzero.dca

# change the label number by frequency 
i = 1
cluster = s1$membership[[ed[i]]]
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
print(table(gr))
cl[[i]] = gr[,-Isolated]

i = 2
cluster = s2$membership[[ed[i]]]
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
print(table(gr))
cl[[i]] = gr[,-Isolated]

i = 3
cluster = s3$membership[[ed[i]]]
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
print(table(gr))
cl[[i]] = gr[,-Isolated]

i = 4
cluster = s4$membership[[ed[i]]]
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
print(table(gr))
cl[[i]] = gr[,-Isolated]
save(cl, file="./data/cluster1.RData")

#### to define shift status, check label stability across stage ####
shift1 = list()
xx = cl[[2]]
cl[[2]][which(xx == 2)] = 3
cl[[2]][which(xx == 3)] = 4
cl[[2]][which(xx == 4)] = 2

i = 1
x = cl[[i]]
y = cl[[i+1]]
intersect(names(x)[which(x == 3)], names(y)[which(y == 3)])
tab = matrix(0, ncol = max(y), nrow = max(x))
for(i in 1:nrow(tab)){
  for(j in 1:ncol(tab)){
    tab[i,j] = length(intersect(names(x)[which(x == i)], names(y)[which(y == j)]))
  }
}
print(tab)
x[which(names(x) %in% setdiff(names(x), names(y)))]

ind.stay = c(intersect(names(x)[which(x == 1)], names(y)[which(y == 1)]),
             intersect(names(x)[which(x == 4)], names(y)[which(y == 4)]), 
             intersect(names(x)[which(x == 3)], names(y)[which(y == 3)])
)
ind.shift = c(intersect(names(x)[which(x == 1)], names(y)[which(y %in% c(2:5))]),
              intersect(names(x)[which(x == 3)], names(y)[which(y %in% c(1,4))]),
              intersect(names(x)[which(x == 4)], names(y)[which(y %in% c(1,5))]),
              intersect(names(x)[which(x == 5)], names(y)[which(y == 4)]),
              intersect(names(x)[which(x == 7)], names(y)[which(y %in% c(1:5))])
)
shift1[[1]] = c(rep(0, length(ind.stay)), rep(1, length(ind.shift)))
names(shift1[[1]]) = c(ind.stay, ind.shift)


xx = cl[[3]]
cl[[3]][which(xx == 4)] = 1
cl[[3]][which(xx == 1)] = 4

i = 2
x = cl[[i]]
y = cl[[i+1]]
intersect(names(x)[which(x == 3)], names(y)[which(y == 3)])
tab = matrix(0, ncol = max(y), nrow = max(x))
for(i in 1:nrow(tab)){
  for(j in 1:ncol(tab)){
    tab[i,j] = length(intersect(names(x)[which(x == i)], names(y)[which(y == j)]))
  }
}
print(tab)
table(x[which(names(x) %in% setdiff(names(x), names(y)))])

ind.stay = c(intersect(names(x)[which(x == 1)], names(y)[which(y == 1)]),
             intersect(names(x)[which(x == 2)], names(y)[which(y == 2)]), 
             intersect(names(x)[which(x == 4)], names(y)[which(y == 4)])
)
ind.shift = c(intersect(names(x)[which(x == 1)], names(y)[which(y %in% c(2:5))]),
              intersect(names(x)[which(x == 2)], names(y)[which(y %in% c(3:5))]),
              intersect(names(x)[which(x == 3)], names(y)[which(y %in% c(1))]),
              intersect(names(x)[which(x == 4)], names(y)[which(y %in% c(1:3))]),
              intersect(names(x)[which(x == 5)], names(y)[which(y == 2)])
)
shift1[[2]] = c(rep(0, length(ind.stay)), rep(1, length(ind.shift)))
names(shift1[[2]]) = c(ind.stay, ind.shift)

xx = cl[[4]]
cl[[4]][which(xx == 2)] = 3
cl[[4]][which(xx == 3)] = 2
cl[[4]][which(xx == 4)] = 5
cl[[4]][which(xx == 5)] = 1
cl[[4]][which(xx == 1)] = 4
i=3
x = cl[[i]]
y = cl[[i+1]]
intersect(names(x)[which(x == 3)], names(y)[which(y == 3)])
tab = matrix(0, ncol = max(y), nrow = max(x))
for(i in 1:nrow(tab)){
  for(j in 1:ncol(tab)){
    tab[i,j] = length(intersect(names(x)[which(x == i)], names(y)[which(y == j)]))
  }
}
print(tab)
table(x[which(names(x) %in% setdiff(names(x), names(y)))])

ind.stay = c(intersect(names(x)[which(x == 1)], names(y)[which(y == 1)]),
             intersect(names(x)[which(x == 3)], names(y)[which(y == 3)]), 
             intersect(names(x)[which(x == 4)], names(y)[which(y == 4)])
)
ind.shift = c(intersect(names(x)[which(x == 1)], names(y)[which(y %in% c(2:6))]),
              intersect(names(x)[which(x == 2)], names(y)[which(y %in% c(1,4,5,6,7))]),
              intersect(names(x)[which(x == 3)], names(y)[which(y %in% c(2,4,5,6))]),
              intersect(names(x)[which(x == 4)], names(y)[which(y %in% c(1:3,5,7))]),
              intersect(names(x)[which(x == 5)], names(y)[which(y %in% c(1:7))])
)
shift1[[3]] = c(rep(0, length(ind.stay)), rep(1, length(ind.shift)))
names(shift1[[3]]) = c(ind.stay, ind.shift)
##################
save(cl, file="./data/cluster2.RData")
save(shift1, file="./data/shift_or_not.RData")

########## plot ###########################
#### define node position ####
load("./data/nonzero.dca.RData")
load("./data/cluster2.RData")
load("./data/shift_or_not.RData")
adj = nonzero.dca
id = rownames(adj[[1]])

load("/Users/zhenwang/Library/CloudStorage/GoogleDrive-zwang9898@gmail.com/My\ Drive/project/dca_net/Data/new.layout.RData")
which(V(g1)$name %in% c("USA", "CHN", "RUS")) # [1]   1  47 124
newl[47,] = c(61,131.8399)
newl[1,] = c(57, 124)
newl[130,] = c(58, 119.5) # india
newl[21,] = c(56, 119.5) #uk
newl[43,] = c(53.97832 ,121.4323)
newl[112,] = c(57, 122)
# consistent with 4 stage plot in paper in color. 
cl1 = cl
cl1[[2]][cl[[2]] == 2] = 3
cl1[[2]][cl[[2]] == 3] = 2
cl1[[4]][cl[[4]]==1] = 4
cl1[[4]][cl[[4]]==4] = 1
#### 4 stage plot ####
st = c(1,5,10,15)
ed = c(4,9,14,21)
pdf("./stage.pdf")
op <- par(mfrow = c(2,2),
          oma = c(0,0,0,0) ,
          mar = c(0,0,0,0) )
for(i in 1:4){
  g1 = graph_from_adjacency_matrix(adj[[ed[i]]], mode="undirected")
  V(g1)$name = countrycode(id, "cown", "cowc")
  Isolated = which(igraph::degree(g1)==0)
  g2 = delete_vertices(g1, Isolated)
  l2 = newl[-Isolated,]
  # with degree
  plot(g2, vertex.size=igraph::degree(g2), vertex.label = V(g2)$name,
       vertex.label.cex=0.5, edge.width = 0.6,
       vertex.color = cl1[[i]], vertex.frame.color="gray35",
       vertex.label.dist=0.5, vertex.label.degree=-pi/2,
       layout=l2)
  # no degree
  # plot(g2, vertex.size=9, vertex.label = V(g2)$name,
  #      vertex.label.cex=0.5, edge.width = 0.6,
  #      vertex.color = gr[-Isolated], vertex.frame.color="gray35",
  #      layout=l2)
}
par(op)
dev.off()

save(cl1, file='./data/cluster_for4stageplot.RData')
save(newl, file="./data/layout.RData")

##### 21 plot #####

load("/Users/zhenwang/Library/CloudStorage/GoogleDrive-zwang9898@gmail.com/My\ Drive/project/dca_net/Data/new.layout.RData")
pdf("./21year.pdf",width=35, height=14)
op <- par(mfrow = c(3,7),mai = c(0.1, 0.1, 0.1, 0.1))
for(i in 1:21){
  for(j in 1:4){
    g1 = graph_from_adjacency_matrix(adj[[i]], mode="undirected")
    V(g1)$name = countrycode(id, "cown", "cowc")
    Isolated = which(igraph::degree(g1)==0)
    g2 = delete_vertices(g1, Isolated)
    l2 = newl[-Isolated,]
    if(i %in% c(st[j]:ed[j])){
      gr = cl1[[j]]
      #print(gr)
      # # with degree
      # plot(g2, vertex.size=igraph::degree(g2), vertex.label = V(g2)$name,
      #      vertex.label.cex=0.5, edge.width = 0.6,
      #      vertex.color = cl[[i]], vertex.frame.color="gray35",
      #      vertex.label.dist=0.5, vertex.label.degree=-pi/2,
      #      layout=l2)
      # no degree
      plot(g2, vertex.size=9, vertex.label = V(g2)$name,
           vertex.label.cex=0.5, edge.width = 0.6,
           vertex.color = cl[[j]], vertex.frame.color="gray35",
           layout=l2)
    }
  }
}
par(op)
dev.off()





