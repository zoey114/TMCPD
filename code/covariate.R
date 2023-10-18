library(dplyr)
library(reshape2) 
library(countrycode)

setwd("/Users/zhenwang/Documents/new_IR/")
## load data 
load("./data/nonzero.dca.RData")
load("./data/layout.RData")
yr = c(1990:2010)
######## clean covariates data ###########
nets.total <- read.csv("./data/01.networks", header=TRUE, row.names=1) # [1] 554258     17
id <- sort(unique(nets.total$ccode1))
length(id) # 164, use all countries
# change id to iso3n ccode. 
id.code = countrycode(id, "cown", "iso3n") 
countrycode(345, "cown", "country.name") # "Yugoslavia" Yugoslavia 
countrycode("Yugoslavia","country.name", "iso3n")
countrycode("Yugoslavia","country.name", "cown")
countrycode(891, "iso3n", "country.name")
id.code[is.na(id.code)] = 891
# id: cluster id; id.code: covariates ccode 
# id.code: consistent with covariates code 
cov = read.csv(file="./data/Covariates/QoGjustwhatisneeded.csv")
colnames(cov)
cov = cov %>% filter(year %in% yr) %>% filter(ccode %in% id.code)
dim(cov)
length(unique(cov[,"ccode"]))

######### change shift ccode to cov ccode #######
load("./data/shift_or_not.RData")
y = shift1
for(i in 1:3){
  names(y[[i]]) = id.code[which(id %in% names(shift1[[i]]))]
}

######### combined with cov, build df #####
st = c(1,5,10,15)
ed = c(4,9,14,21)
x = list() 
df = c()
for(i in 1:3){
  x[[i]] = cov %>% filter(year %in% yr[c(st[i]:ed[i])]) %>% 
    filter(ccode %in% names(y[[i]])) 
  na.per = unlist(lapply(5:21, function(jj){sum(is.na(x[[i]][,jj]))/nrow(x[[i]])}))
  names(na.per) = colnames(cov)[5:21]
  df = cbind(df, na.per)
}
print(df) # remove variables that contains much NA elements. 
x.avg = list()
for(i in 1:3){
  x[[i]] = x[[i]][,-c(14:17)]
  ccode = unique(x[[i]][,1])
  x.avg[[i]] = matrix(0, nrow=length(ccode), ncol = 14)
  for(j in 1:length(ccode)){
    df = x[[i]][which(x[[i]]$ccode == ccode[j]),]
    df = df[,-c(2,3,4)]
    x.avg[[i]][j,] = colMeans(df, na.rm=TRUE)
  }
  colnames(x.avg[[i]]) = colnames(df)
}

df = as.data.frame(cbind(names(y[[1]]), y[[1]]))
df = cbind(df, stage = "s1")
colnames(df) = c("ccode", "shift_or_not", "stage")
for(i in 2:3){
  tt = as.data.frame(cbind(names(y[[i]]), y[[i]]))
  colnames(tt) = c("ccode", "shift_or_not")
  tt = cbind(tt, stage=paste0("s",i))
  colnames(tt) = colnames(df)
  df = rbind(df, tt)
}
y.til = df
df = cbind(x.avg[[1]], stage = "s1")
for(i in 2:3){
  tt = cbind(x.avg[[i]], stage=paste0("s",i))
  df = rbind(df, tt)
}
x.avg.til = df
dat = merge(y.til, x.avg.til, by=c("ccode", "stage"))
for(j in 3:ncol(dat)){
  dat[,j] = as.numeric(dat[,j])
}
## add network structure variate 
nets.dat = nets.total[,1:4]
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
degree_results <- list()
pagerank_results <- list()
centrality_results <- list()
for (i in c(1:21)) {
  year = yr[i]
  graph <-graph.adjacency(dca[[i]], mode = "undirected") 
  degree_results[[i]] <- igraph::degree(graph)
  pagerank_results[[i]] <- page_rank(graph)$vector
  centrality_results[[i]] <- igraph::closeness(graph)
}
st = c(1,5,10,15)
ed = c(4,9,14,21)
deg = list()
pr = list()
cen = list()
for (j in 1:3){
  tt = rowMeans(do.call(cbind, degree_results[st[j]:ed[j]]))
  names(tt) = id.code
  deg[[j]] = tt
  tt = rowMeans(do.call(cbind, pagerank_results[st[j]:ed[j]]))
  names(tt) = id.code
  pr[[j]] = tt
  tt =  rowMeans(do.call(cbind, centrality_results[st[j]:ed[j]]))
  names(tt) = id.code
  cen[[j]] = tt
}
deg = do.call(cbind, deg)
colnames(deg) = c("s1","s2","s3")
deg = melt(deg, id.vars = "Name", variable.name = "Variable", value.name = "Value")
colnames(deg) = c("ccode","stage","deg")
pr = do.call(cbind, pr)
colnames(pr) = c("s1","s2","s3")
pr = melt(pr, id.vars = "Name", variable.name = "Variable", value.name = "Value")
colnames(pr) = c("ccode","stage","pr")
cen = do.call(cbind, cen)
colnames(cen) = c("s1","s2","s3")
cen = melt(cen, id.vars = "Name", variable.name = "Variable", value.name = "Value")
colnames(cen) = c("ccode","stage","cen")
df = merge(deg, pr, by = c("ccode","stage"))
df = merge(cen,df, by=c("ccode","stage"),all=TRUE)
dat = merge(dat, df,  by=c("ccode","stage"),all=TRUE)
dat <- dat[complete.cases(dat$shift_or_not), ]
save(dat, file="./data/dat.RData")

######### Figure 10: Descriptive analysis of covariates ##########
# change var name to full name 
var_short = colnames(dat)[4:19]
var_full = c("Number of Alliances ",
             "Global Militarization Index ",
             "Heavy Weapons Index ",
             "Military Expenditure Index",
             "Military Personnel Index",
             "Did the main regime change",
             "Level of Democracy (Freedom House/Polity)",
             "Real GDP per Capita (2005)",
             "ICRG Indicator of Quality of Government",
            " Political corruption index",
            " Deliberative democracy index",
             "Egalitarian democracy index",
            " Liberal democracy index",
            "Centrality ",
             "Degree ",
            " PageRank "
)
colnames(dat)[4:19] = var_full 
# box plot 
xx = dat %>% dplyr::filter(stage == "s1") 
xx[, 4:ncol(xx)] = scale(xx[, 4:ncol(xx)], center = TRUE, scale = TRUE)
par(mfrow=c(4,4),mai=c(0.5,0.5,0.5,0.5))
for (i in 4:ncol(xx)){
  var = colnames(xx)[i]
  x1 = xx[which(xx[,"shift_or_not"] == 0), i]
  x2 = xx[which(xx[,"shift_or_not"] == 1), i]
  x1 = na.omit(x1)
  x2 = na.omit(x2)
  boxplot(x1, x2, col= c("darkgray", "lightgray"),main = var, cex.main=1)
}









xx = dat %>% dplyr::filter(stage == "s1") 
xx[, 4:ncol(xx)] = scale(xx[, 4:ncol(xx)], center = TRUE, scale = TRUE)
par(mfrow=c(4,4),mai=c(0.5,0.5,0.5,0.5))
for (i in 4:ncol(xx)){
  var = colnames(xx)[i]
  x1 = xx[which(xx[,"shift_or_not"] == 0), i]
  x2 = xx[which(xx[,"shift_or_not"] == 1), i]
  x1 = na.omit(x1)
  x2 = na.omit(x2)
  boxplot(x1, x2, main = paste0("var = ", var), xlab = "shift_or_not")
}







