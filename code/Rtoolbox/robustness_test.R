library(igraph)

## this function takes the edgelist and perturb a fixed percentage of the edges
perturb<-function(list,percentage){
	m<-round(dim(list)[1]*percentage)
	i=0
	while(i<m){
		g1<-graph.edgelist(list,directed=F)
		n<-dim(list)[1]
		int<-sample(n,2,replace=FALSE)
		while(length(unique(as.vector(list[int,])))<4 || are.connected(g1,list[int[1],1],list[int[2],1]) || are.connected(g1,list[int[1],2],list[int[2],2])){
			int<-sample(n,2,replace=FALSE)
			}
		if(sample(c(0,1),1)){
			p<-list[int[1],2]
			list[int[1],2]<-list[int[2],1]
			list[int[2],1]<-p
			i<-i+2
		}
		else{
			list<-list[-int[1],]
			ind<-sample(length(V(g1)),2,replace=FALSE)
			while(are.connected(g1,ind[1],ind[2])){
				ind<-sample(length(V(g1)),2,replace=FALSE)
			}
			list<-rbind(list,ind)
			i<-i+1
		}
	}
	list
}

calculate_perturb_MMatrix<-function(size,n,percentage){
	M<-matrix(0,size,size)
	for(i in 1:n){
		A1<-read.csv(paste("A",i,".csv",sep=""),header=F)
		A1<-as.matrix(A1)
		diag(A1)<-0
		G<-graph.edgelist(perturb(get.edgelist(graph.adjacency(A1,mode="max"),names=F),percentage),directed=F)
		if(length(V(G))==(size-1)){
			G<-add.vertices(G,1)
		}
		A<-get.adjacency(G,type="both",sparse=F)
		M<-M+(A-degree(G)%*%t(degree(G))/sum(degree(G)))
	}
	M
}



## this function generates perturbed network, obtain the membership_perturb and calculate 
## the nmi between the membership_perturb and the membership from the observed network

robust_test<-function(n_sample,size,n,percentage){
    robust_vec<-c()
	for(i in 1:n_sample){
		setwd('~/Rtool/data_example')                              #change this to the path for downloaded 'Rtool/data_example'
		M<-calculate_perturb_MMatrix(size,n,percentage)
		setwd('~/Rtool')                                           #change this to the path for downloaded 'Rtool'
		write.table(M,"Mperturb.csv",row.names=FALSE,col.names=FALSE,sep=",",append=TRUE)
	}
	code<-c(                                                       #this is the code for louvain method 
	"newFolder='/Rtool';",                                         #set this to the path of the downloaded 'Rtool' file
	"cd(newFolder)",
	paste("for i=1:",n_sample,sep=""),
	paste("M=csvread('Mperturb.csv',(i-1)*",size,",0,[(i-1)*",size," 0 i*",size,"-1 ",size,"-1]);",sep=""),
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
    "dlmwrite('mem_perturb.csv',s0,'-append')",
    "end")
	run_matlab_code(code)
	t(read.csv("mem_perturb.csv",header=F))
}


####example
### the directory needs to be specified at line 57, 59 and 63
mem<-robust_test(10,100,10,0.1)
membership<-t(read.csv("membership.csv",header=F))
p1<-c()
for(i in 1:10){
	cluster<-mem[(1+(i-1)*size):(size*i)]
	p1<-c(p1,compare(membership,cluster,method="nmi"))

}




