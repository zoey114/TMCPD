setwd("/Users/zhenwang/Documents/new_IR/")
load("./data/cluster2.RData")
###### Middle East Countries #####
ME.countryname = c("Cyprus", "Lebanon", "Syria", "Iraq", "Iran", "Israel", 
                   "Jordan", "Saudi Arabia", "Kuwait", "Qatar", "Bahrain", "United Arab Emirates", "Oman", "Yemen")
library("countrycode")
ME.cown = countrycode(ME.countryname, "country.name", "cown")
ME.cown = intersect(ME.cown, id)
ME.cown = as.numeric(ME.cown)
countrycode(ME.cown, "cown", "country.name")

# [1] "Lebanon"              "Syria"                "Iraq"                
# [4] "Iran"                 "Israel"               "Jordan"              
# [7] "Saudi Arabia"         "Kuwait"               "Qatar"               
# [10] "United Arab Emirates" "Oman"                 "Yemen"   

##################### only ME countries metwork #############
load("./data/nonzero.dca.RData")
adj = nonzero.dca
id = rownames(adj[[1]])
adj.ME = lapply(adj, function(df) df[rownames(df)%in%ME.cown, colnames(df)%in%ME.cown])
load("./data/layout.RData")
newl.ME = newl[id%in%ME.cown,]
# remove isolated node 

## 21 year plot ##
library(igraph)
yr = seq(1990,2010)
pdf("./ME.21_1year.pdf",width=35, height=14)
op <- par(mfrow = c(3,7),mai = c(1, 1, 1, 1))
for(i in 1:21){
  #for(j in 1:4){
    g1 = graph_from_adjacency_matrix(adj.ME[[i]], mode="undirected")
    V(g1)$name = countrycode(ME.cown, "cown", "cowc")
    Isolated = which(igraph::degree(g1)==0)
    g2 = delete_vertices(g1, Isolated)
    l2 = newl.ME[-Isolated,]
    plot(g2, vertex.size=20, vertex.label = V(g2)$name,
                 vertex.label.cex=3, edge.width = 1.5,vertex.label.dist=2,edge.color="black",
                 vertex.color = "steelblue1", vertex.frame.color="gray35",
                 layout=l2)
    title(paste0("year:",yr[i]),cex.main=3,col.main="black")
    # if(i %in% c(st[j]:ed[j])){
    #   gr = cl1[[j]]
    #   plot(g2, vertex.size=9, vertex.label = V(g2)$name,
    #        vertex.label.cex=0.5, edge.width = 0.6,
    #        vertex.color = cl[[j]], vertex.frame.color="gray35",
    #        layout=l2)
    # }
  #}
}
par(op)
dev.off()
### change point detection ###
yr = seq(1990,2010)
res = mod_cluster(Adj = adj.ME, yr, start=8) 
setwd("/Users/zhenwang/Documents/new_IR/")
save(res, file = "./data/ME.mod1-21.RData")
mod = res$modularity[8:21]
diff_mod = diff(mod)
library(EnvCpt)
fit_envcpt = envcpt(diff_mod)  # Fit all models at once
fit_envcpt$summary  # Show log-likelihoods
plot(fit_envcpt)
fit_envcpt = envcpt(mod, models="meancpt") 
plot(fit_envcpt)
cp.location = fit_envcpt$meancpt@cpts 
cp.location #[1]  9 14
# 1998-2006, 2002-2010
# # start at 6: 
# res1 = mod_cluster(Adj = adj.ME, yr, start=13) 
# setwd("/Users/zhenwang/Documents/new_IR/")
# save(res1, file = "./data/ME.mod13-21.RData")
# mod = res1$modularity[13:21]
# fit_envcpt = envcpt(mod)  # Fit all models at once
# fit_envcpt$summary  # Show log-likelihoods
# plot(fit_envcpt)
# fit_envcpt = envcpt(mod, models="meancpt") 
# cp.location = fit_envcpt$meancpt@cpts 
# cp.location # 5 
cl.ME = res$membership[8:21]
load("./data/layout.RData")
id = as.numeric(rownames(nonzero.dca[[1]]))
newl.ME = newl[id%in%ME.cown,]
st = c(8,17)
ed = c(16,21)
pdf("./ME_2stage.pdf")
op <- par(mfrow = c(1,2) )
for (j in 1:2){
  g1 = graph_from_adjacency_matrix(adj.ME[[ed[j]]], mode="undirected")
  V(g1)$name = countrycode(ME.cown, "cown", "cowc")
  plot(g1, vertex.size=igraph::degree(g1)*8, vertex.label = V(g1)$name,
       vertex.label.cex=0.5, edge.width = 0.6,
       vertex.color = cl.ME[[j]], vertex.frame.color="gray35",
       vertex.label.dist=0.5, vertex.label.degree=-pi/2,
       layout=newl.ME)
}
par(op)
dev.off()

pdf("./ME_2stage_nodegree.pdf")
op <- par(mfrow = c(1,2) )
for (j in 1:2){
  g1 = graph_from_adjacency_matrix(adj.ME[[ed[j]]], mode="undirected")
  V(g1)$name = countrycode(ME.cown, "cown", "cowc")
  plot(g1, vertex.size=8, vertex.label = V(g1)$name,
       vertex.label.cex=0.5, edge.width = 0.6,
       vertex.color = cl.ME[[j]], vertex.frame.color="gray35",
       vertex.label.dist=0.5, vertex.label.degree=-pi/2,
       layout=newl.ME)
}
par(op)
dev.off()

s1 = cl.ME[[9]]
s2 = cl.ME[[14]]
df = data.frame(rbind(s1,s2))
colnames(df) = countrycode(ME.cown, "cown", "cowc")
rownames(df) = c("stage1","stage2")
print(df)

data <- data.frame(
  LEB = c(1, 1),
  SYR = c(2, 1),
  IRQ = c(1, 1),
  IRN = c(3, 1),
  ISR = c(4, 2),
  JOR = c(5, 3),
  SAU = c(6, 4),
  KUW = c(7, 2),
  QAT = c(1, 1),
  UAE = c(8, 1),
  OMA = c(9, 5),
  YEM = c(1, 1)
)
# Create column names
colnames(data) <- c(
  "LEB", "SYR", "IRQ", "IRN", "ISR", "JOR", 
  "SAU", "KUW", "QAT", "UAE", "OMA", "YEM"
)
# Write the data frame to a CSV file
write.csv(data, "./data/ME_cluster.csv", row.names = FALSE)

##################### Robustness test previous #################
load("./data/layout.RData")
st = c(1,5,10,15)
ed = c(4,9,14,21)
adj = nonzero.dca
ME.cl = lapply(cl, function(df) df[names(df)%in%ME.cown])
adj.ME = lapply(adj, function(df) df[rownames(df)%in%ME.cown, colnames(df)%in%ME.cown])
newl.ME = newl[id%in%ME.cown,]
#pdf("/Users/zhenwang/Downloads/stage_ME.pdf")
op <- par(mfrow = c(2,2) )
for(i in 1:4){
  g1 = graph_from_adjacency_matrix(adj.ME[[ed[i]]], mode="undirected")
  V(g1)$name = countrycode(ME.cown, "cown", "cowc")
  #Isolated = which(igraph::degree(g1)==0)
  #g2 = delete_vertices(g1, Isolated)
  #l2 = newl.ME[-Isolated,]
  # with degree
  plot(g1, vertex.size=igraph::degree(g1)*5, vertex.label = V(g1)$name,
       vertex.label.cex=0.5, edge.width = 0.6,
       vertex.color = cl[[i]], vertex.frame.color="gray35",
       vertex.label.dist=0.5, vertex.label.degree=-pi/2,
       layout=newl.ME)
  # no degree
  # plot(g2, vertex.size=9, vertex.label = V(g2)$name,
  #      vertex.label.cex=0.5, edge.width = 0.6,
  #      vertex.color = gr[-Isolated], vertex.frame.color="gray35",
  #      layout=l2)
}
par(op)
#dev.off()

#### add big countries #####
ME.countryname = c("Cyprus", "Lebanon", "Syria", "Iraq", "Iran", "Israel", 
                   "Jordan", "Saudi Arabia", "Kuwait", "Qatar", "Bahrain", "United Arab Emirates", "Oman", "Yemen")
ME.cown = countrycode(ME.countryname, "country.name", "cown")
ME.cown = intersect(ME.cown, id)
big.id = countrycode(c("RUS","USA","CHN"),"cowc","cown")
ME.cown = c(ME.cown, big.id)
ME.cown = sort(ME.cown)

ME.cl = lapply(cl, function(df) df[names(df)%in%ME.cown])
adj.ME = lapply(adj, function(df) df[rownames(df)%in%ME.cown, colnames(df)%in%ME.cown])
newl.ME = newl[id%in%ME.cown,]
pdf("./stage_ME_big0.pdf")
op <- par(mfrow = c(2,2),     
          oma = c(1,1,1,1),
          mar = c(1,1,1,1) )
for(i in 1:4){
  g1 = graph_from_adjacency_matrix(adj.ME[[ed[i]]], mode="undirected")
  g2 = graph_from_adjacency_matrix(adj[[ed[i]]], mode="undirected")
  V(g1)$name = countrycode(ME.cown, "cown", "cowc")
  #Isolated = which(igraph::degree(g1)==0)
  #g2 = delete_vertices(g1, Isolated)
  #l2 = newl.ME[-Isolated,]
  # with degree
  #plot(g1, vertex.size=igraph::degree(g1)*5, vertex.label = V(g1)$name,
  #    vertex.label.cex=0.5, edge.width = 0.6,
  #   vertex.color = cl[[i]], vertex.frame.color="gray35",
  #   vertex.label.dist=0.5, vertex.label.degree=-pi/2,
  #  layout=newl.ME)
  # no degree
  plot(g1, vertex.size=igraph::degree(g1)*8, vertex.label = V(g1)$name,
       vertex.label.cex=0.5, edge.width = 0.6,
       vertex.color =  cl[[i]], vertex.frame.color="gray35",
       layout=newl.ME)
}
par(op)
dev.off()

