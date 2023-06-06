setwd("/Users/anandr/DSG/DSG_Project")
data1 <- read.csv("./normalized.csv", row.names = 1)
print(data)
corr_list=list()
for (iter in 1:50){
  data <- data1[sample(1:nrow(data1), 1000),]
  corr_file=cor(data,method='spearman')
  corr_file[is.na(corr_file)]=0
  X=corr_file
  ind <- which(upper.tri(X, diag = FALSE), arr.ind = TRUE)
  nn <- dimnames(X)
  X2=data.frame(row = nn[[1]][ind[, 1]],
                col = nn[[2]][ind[, 2]],
                val = X[ind])
  corr_list[[iter]]=X2
}
net<-Reduce(function(x, y) merge(x=x, y=y, by=c("row","col"), all.x=F, all.y=F), corr_list)
#write.table(net,filename,sep='\t',quote=FALSE,row.names=FALSE)


library(stats)
#pca_res <- prcomp(net[,3:52])
# Save the dataframe as a CSV file
write.csv(net, "net.csv", row.names = FALSE)
net <- read.csv("./net.csv", row.names = 1)




# SVD CODE

smat = svd(net[,3:52])

U=smat$u
V=smat$v
D <- diag(smat$d)

U1 <- as.matrix(U[,1:5])
d1 <- as.matrix(D[1:5,1:5])
V1 <- as.matrix(V[,1:5])

abc1 <- U1 %*% d1 %*% t(V1)
write.csv(abc1, "abc1.csv", row.names = FALSE)
#write.table(abc1, file = filename, sep="\t", quote=F)

options(scipen=999)
abc2=as.matrix(apply(abc1,1,median))
abc2=abs(abc2[,1])
abc3=cbind(net[,c(1:2)],abc2)
abc3=abc3[order(abc3$abc2,decreasing = T),]
write.csv(abc3, "abc3.csv", row.names = FALSE)
abc4=abc3[1:50,]
write.csv(abc4, "abc4.csv", row.names = FALSE)

# calculate eigenvalues and eigenvectors
eig <- eigen(abc1)

# print eigenvalues
eig$values

# print eigenvectors
eig$vectors

A = net[,3:52]
A = apply(A, 2, as.numeric)
A.svd = svd(net[,3:52])
A.svd
ATA <- t(A) %*% A
ATA
ATA.e <- eigen(ATA)
v.mat <- ATA.e$vectors
v.mat
# Install igraph
install.packages("igraph")

# Load igraph
library(igraph)

