getwd()
library("fpc")
library("cluster")
library("bootcluster")
library("fossil")

seeds <- read.delim("C:/shreyas/document/books/statistical_data_mining/seeds.txt")
head(seeds)
dim(seeds)
seed <- seeds[,1:7]
seed_dist <- dist(seed)
complete_hc <- hclust(seed_dist,method="complete")
single_hc <- hclust(seed_dist,method="single")
average_hc <- hclust(seed_dist,method="average")


plot(complete_hc,hang = -1,main="Complete Linkage")
plot(single_hc,hang = -1,main="Single Linkage")
plot(average_hc,hang = -1,main="Average Linkage")

store <-c()
for(i in 2:6){
  ct <- cutree(average_hc,k=i)
  si <- silhouette(ct,dist=seed_dist)
  avg_width <- summary(si)$avg.width
  store <- c(store,avg_width)
}

complete_cut =  cutree(complete_hc,k=3)
t_complete = table(complete_cut,seeds$Seed.Group)
t_complete
single_cut =  cutree(single_hc,k=3)
t_single = table(single_cut,seeds$Seed.Group)
t_single
average_cut =  cutree(average_hc,k=3)
t_average = table(average_cut,seeds$Seed.Group)
t_average

adj_complete = adj.rand.index(complete_cut,as.numeric(seeds$Seed.Group))
adj_complete
adj_single = adj.rand.index(single_cut,as.numeric(seeds$Seed.Group))
adj_single
adj_average = adj.rand.index(average_cut,as.numeric(seeds$Seed.Group))
adj_average

#-----------Clustering based on K-means---------------------------------

#To find the best number of cluster
k.select(seed, range = 2:6, B=50, r= 5, scheme_2=TRUE)

k.select(seed, range = 2:6, B=50, r= 5, scheme_2=FALSE)


km = kmeans(seed,centers=3,nstart=20)


gap_kmeans <- clusGap(seed, kmeans, nstart = 20, K.max = 10, B = 100)

table(km$cluster,seeds$Seed.Group)

adj_kmeans <- adj.rand.index(km$cluster,as.numeric(seeds$Seed.Group))
adj_kmeans
