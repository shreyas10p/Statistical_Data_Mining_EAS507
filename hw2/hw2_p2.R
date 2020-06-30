getwd()

## ----------Question 2 (a)--------------------------------------------------------------
data_set = read.csv("C:/shreyas/document/books/statistical_data_mining/Ch10Ex11.csv", header = FALSE)


## ----------Question 2 (b)--------------------------------------------------------------

method_list = list('complete','single','average')

#Complete Linkage
complete_hclust = hclust(as.dist(1 - cor(data_set)),method = method_list[1])              
plot(complete_hclust,main='Dendogram for complete',col="mediumturquoise")

#Single Linkage
single_hclust = hclust(as.dist(1 - cor(data_set)),method = method_list[2])
plot(single_hclust,main='Dendogram for single',col="dark red ")

#Average Linkage
average_hclust = hclust(as.dist(1 - cor(data_set)),method = method_list[3])
plot(average_hclust,main='Dendogram for average',col="blue")



#----------------Question2(c)---------------------------------------------
#K-means
kmean =  kmeans(t(data_set), centers=2)
kmean$cluster


kmean2 = kmeans(data_set, centers=2, nstart=20)
kmean2$cluster

pcomp2 = prcomp(data_set, center=TRUE, scale. = TRUE)

par(mfrow=c(2,2))
biplot(pcomp2)
plot(pcomp2$x[,1], pcomp2$x[,2], col=kmean2$cluster, pch=16, xlab="PComponent 1", ylab="PComponent 2")






