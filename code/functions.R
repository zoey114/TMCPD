
######## define change point detection function: from start to the wnd of time years #######################

mod_cluster = function(Adj, yr, start){
  library(igraph)
  library(matlabr)
  library(parallel)
  options(matlab.path = "/Applications/MATLAB_R2021b.app/bin")   #set up the path for matlab, i.e. where matlab is located
  have_matlab()   
  setwd("/Users/zhenwang/Documents/new_IR/code/Rtoolbox")
  modularity = c()
  membership = list()
  id = rownames(Adj[[1]])
  for(time in c(start:length(yr))){
    ### calculate MMatrix 
    size = length(id)
    n = time
    #lap = 5
    M<-matrix(0,size,size)
    for(i in start:time){
      A1<-Adj[[i]]
      A1<-as.matrix(A1)
      if(dim(A1)[1]==size & dim(A1)[2]==size){
        diag(A1)<-0
        G<-graph.adjacency(A1,mode="max")
        A<-get.adjacency(G,type="both",sparse=F)
        M<-M+(A-igraph::degree(G)%*%t(igraph::degree(G))/sum(igraph::degree(G)))
      }
      else{
        print("networks are not the same size")
      }
    }
    calculate_norm<-function(size,n){     
      #this function calculates the normalizing constant 2*\hat{m}
      sum<-0
      for(i in start:time){
        A1<-Adj[[i]]
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
      "newFolder='.';",     #change this to the path of the downloaded 'Rtool' file
      "cd(newFolder)",
      "M=csvread('MMatrix_dca.csv',1,0);",
      "cd(newFolder)",
      "cd('./GenLouvain2.0')",
      "[s0,q0]=genlouvain(M,10000,0);",
      "for i=1:1000",        #apply the louvain method 1000 times and output the one with the largest modularity value
      "[s,q]=genlouvain(M,10000,0);",
      "if q>q0",
      "q0=q;",
      "s0=s;",
      "end",
      "end",
      "cd(newFolder)",
      "csvwrite('membership_dca.csv',s0)",
      "csvwrite('modularity_dca.csv',full(q0))"
    )
    
    Louvain<-function(code){  #this outputs the membership file and the maximum modularity value to path=newFolder
      run_matlab_code(code)
    }
    
    
    # setwd('./Rtoolbox/data_example/')     #set this to the downloaded 'Rtool/data_example'
    # M<-calculate_MMatrix(100,10)
    sum<-calculate_norm(size,n)
    # setwd('..')                           #set this to the downloaded 'Rtool' file
    write.csv(M,"MMatrix_dca.csv",row.names=F)
    Louvain(code)
    modularity[time]<-as.numeric(read.csv("./Genlouvain2.0/modularity_dca.csv",header=F))/sum
    membership[[time]]<-t(read.csv("./Genlouvain2.0/membership_dca.csv",header=F))
  }
  res = list('modularity' = modularity,'membership' = membership, 'adj' = Adj)
  return(res)
}

