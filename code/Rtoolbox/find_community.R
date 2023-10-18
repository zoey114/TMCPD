#install.packages("igraph")                                   #the following code requires R pacakge 'igraph'
#install.packages("matlabr")                                  #the following code requires R package 'matlabr'
library(igraph)
library(matlabr)
options(matlab.path = "/Applications/MATLAB_R2015b.app/bin")   #set up the path for matlab, i.e. where matlab is located
have_matlab()                                                  #this result indicates if matlab can be called
                                                               #the output should be TRUE


calculate_MMatrix<-function(size,n){                           #this function calculates the modularity matrix. Here size is the number of nodes in the network and n is the number of snapshots.
	M<-matrix(0,size,size)
	for(i in 1:n){
		A1<-read.csv(paste("A",i,".csv",sep=""),header=F)
		A1<-as.matrix(A1)
		if(dim(A1)[1]==size & dim(A1)[2]==size){
			diag(A1)<-0
			G<-graph.adjacency(A1,mode="max")
			A<-get.adjacency(G,type="both",sparse=F)
			M<-M+(A-degree(G)%*%t(degree(G))/sum(degree(G)))
			}
			else{
				print("networks are not the same size")
				}
	}
	M
}

calculate_norm<-function(size,n){                             #this function calculates the normalizing constant 2*\hat{m}
	sum<-0
	for(i in 1:n){
		A1<-read.csv(paste("A",i,".csv",sep=""),header=F)
		A1<-as.matrix(A1)
		if(dim(A1)[1]==size & dim(A1)[2]==size){
			diag(A1)<-0
			G<-graph.adjacency(A1,mode="max")
			A<-get.adjacency(G,type="both",sparse=F)
			sum<-sum+sum(A)
			}
			else{
				print("networks are not the same size")
				}
	}
	sum
}


#this is the code for louvain method
code<-c(                                                        
"newFolder='/Rtool';",                                         #change this to the path of the downloaded 'Rtool' file
"cd(newFolder)",
"M=csvread('MMatrix.csv',1,0);",
"cd(newFolder)",
"cd('./GenLouvain2.0')",
"[s0,q0]=genlouvain(M,10000,0);",
"for i=1:1000",                                                #apply the louvain method 1000 times and output the one with the largest modularity value
"[s,q]=genlouvain(M,10000,0);",
"if q>q0",
"q0=q;",
"s0=s;",
"end",
"end",
"cd(newFolder)",
"csvwrite('membership.csv',s0)",
"csvwrite('modularity.csv',full(q0))"
)
	
Louvain<-function(code){                                      #this outputs the membership file and the maximum modularity value to path=newFolder
	run_matlab_code(code)
	}




### example
### the directory needs to be specified at line 49, 76 and 79
setwd('~/Rtool/data_example')                                 #set this to the downloaded 'Rtool/data_example'
M<-calculate_MMatrix(100,10)
sum<-calculate_norm(100,10)
setwd('~/Rtool')                                              #set this to the downloaded 'Rtool' file
write.csv(M,"MMatrix.csv",row.names=F)
Louvain(code)
modularity<-as.numeric(read.csv("modularity.csv",header=F))/sum
membership<-t(read.csv("membership.csv",header=F))