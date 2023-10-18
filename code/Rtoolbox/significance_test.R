library(igraph)

## this function generates one sample from the null model using MCMC 
MCMC<-function(d){                    
    edges<-get.edgelist(degree.sequence.game(d, method = "simple.no.multiple"))
    n<-dim(edges)[1]
	for(i in 1:500){                               ## the number of rewiring steps before we take a sample
	g1<-graph.edgelist(edges,directed=F)
	int<-sample(n,2,replace=FALSE)
	while(length(unique(as.vector(edges[int,])))<4 || are.connected(g1,edges[int[1],1],edges[int[2],1]) || are.connected(g1,edges[int[1],2],edges[int[2],2])){
		int<-sample(n,2,replace=FALSE)
	}
	p<-edges[int[1],2]
	edges[int[1],2]<-edges[int[2],1]
	edges[int[2],1]<-p
	i<-i+1
	}
	edges
}

##this function finds the degree sequence at each time point
findDegree<-function(size,n){                      ## n is the number of snapshots and size is the number of nodes
	Degree<-matrix(0,n,size)    
	for(i in 1:n){
		A1<-read.csv(paste("A",i,".csv",sep=""),header=F)
		A1<-as.matrix(A1)
		diag(A1)<-0
		G<-graph.adjacency(A1,mode="max")
		Degree[i,]<-degree(G)
	} 
	Degree
}                         


calculate_MCMC_MMatrix<-function(size,n,Degree){                           #this function calculates the modularity matrix. n is the number of snapshots and size is the number of nodes.
	M<-matrix(0,size,size)
	for(i in 1:n){
		g<-graph.edgelist(MCMC(Degree[i,]),directed=F)
		A<-get.adjacency(g,type="both",sparse=F)
		M<-M+(A-degree(g)%*%t(degree(g))/sum(degree(g)))
	}
	M
}

	
	

##this generates n_sample samples from the null model for the significance test
getnull<-function(n_sample,size,n,Degree){
    mod_null_vec<-c()
	for(i in 1:n_sample){
		M<-calculate_MCMC_MMatrix(size,n,Degree)
		write.table(M,"Mnull.csv",row.names=FALSE,col.names=FALSE,sep=",",append=TRUE)
	}
	code<-c(                                                       #this is the code for louvain method 
	"newFolder='/Rtool';",                                         #change this to the path of the downloaded 'Rtool' file
	"cd(newFolder)",
	paste("for i=1:",n_sample,sep=""),
	paste("M=csvread('Mnull.csv',(i-1)*",size,",0,[(i-1)*",size," 0 i*",size,"-1 ",size,"-1]);",sep=""),
    "cd(newFolder)",
    "cd('./GenLouvain2.0')",
    "[s0,q0]=genlouvain(M,10000,0);",
    "for j=1:1000",                                                #apply the louvain method 1000 times and output the one with the largest modularity value
    "[s,q]=genlouvain(M,10000,0);",
    "if q>q0",
    "q0=q;",
    "s0=s;",
    "end",
    "end",
    "cd(newFolder)",
    "dlmwrite('modnull.csv',full(q0),'-append')",
    "end")
	run_matlab_code(code)
	t(read.csv("modnull.csv",header=F))
}




###Example
### the directory needs to be specified at line 56, 82 and 84
setwd('~/Rtool/data_example')                                       #set this to the path of the downloaded 'Rtool/data_example' file
Degree<-findDegree(100,10)
setwd('~/Rtool')                                                    #set this to the path of the downloaded 'Rtool' file
mod_null_vec<-getnull(10,100,10,Degree)
observe_mod<-as.numeric(read.csv("modularity.csv",header=F))
pvalue<-sum(observe_mod<mod_null_vec)/length(mod_null_vec)
pvalue







