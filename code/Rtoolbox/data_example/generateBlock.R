blocks<-function(n,p,symm=0){
	if(symm==0){
		A<-matrix(sample(c(0,1),n^2,prob=c(1-p,p),replace=T),nrow=n,ncol=n)
	}
	if(symm==1){
		A<-matrix(0,n,n)
		for(i in 1:n){
			A[i,]=A[,i]=sample(c(0,1),n,prob=c(1-p,p),replace=T)
		}
	}
	A
}


p<-matrix(0,4,4)
p[1,]<-c(0.2,0.1,0.1,0.1)
p[2,]<-c(0.1,0.2,0.1,0.1)
p[3,]<-c(0.1,0.1,0.2,0.1)
p[4,]<-c(0.1,0.1,0.1,0.2)

generateA<-function(p){
	A11<-blocks(25,p[1,1],1)
	A12<-blocks(25,p[1,2],0)
	A13<-blocks(25,p[1,3],0)
	A14<-blocks(25,p[1,4],0)
	A21<-t(A12)
	A22<-blocks(25,p[2,2],1)
	A23<-blocks(25,p[2,3],0)
	A24<-blocks(25,p[2,4],0)
	A31<-t(A13)
	A32<-t(A23)
	A33<-blocks(25,p[3,3],1)
	A34<-blocks(25,p[3,4],0)
	A41<-t(A14)
	A42<-t(A24)
	A43<-t(A34)
	A44<-blocks(25,p[4,4],1)
	A<-rbind(cbind(A11,A12,A13,A14),cbind(A21,A22,A23,A24),cbind(A31,A32,A33,A34),cbind(A41,A42,A43,A44))
	diag(A)<-0
	A
}

for(i in 1:10){
	A<-generateA(p)
	write.table(A,paste("A",i,".csv",sep=""),sep=",",row.names=FALSE,col.names=FALSE)	
}