
library(kohonen)
#nci data set in global environment
getwd()
data(nci)                   # data-set downloaded and  loaded directly from the from the file 
head(nci)

#scaling the data
nci.scaled <- scale(nci)

#Hexagonal grid structure
set.seed(7)
som_grid <- somgrid(xdim = 8, ydim = 8, topo = "hexagonal")
# Performing SOM on the scaled data
nci.som <- som(nci.scaled, grid = som_grid, rlen = 2000)


x11()
plot(nci.som, main = "NCI Data")

nci.som$unit.classif

#Changes Plot
x11()
plot(nci.som,type="changes", main = "NCI Data")

# Mapping Plot
x11()                  
plot(nci.som,type="mapping")

#color Palette
colorPalette <- function(n, alpha = 1){rainbow(n, end=7/10, alpha = alpha)[n:1]}

#U-Matrix Visualisation
x11()
plot(nci.som, type = "dist.neighbours",palette.name = colorPalette)   


ncicodes<-nci.som$codes[[1]]

#Hierarchical clustering with complete linkage
distance <- dist(ncicodes)
nci.hclust <- hclust(distance)

#Dendogram for hierarchical clustering
x11()
plot(nci.hclust,labels=colnames(nci),main='Dendogram NCI',col="black")

#Cut the dendogram into 3 clusters
groups.3<-cutree(nci.hclust,3)

#Group 1, Group2 and Group3 Data 
colnames(nci)[groups.3==1]
colnames(nci)[groups.3==2]
colnames(nci)[groups.3==3]

table(groups.3)

colr <- c("red", "blue", "yellow")
bgcolumns <- colr[groups.3]

graphics.off()

x11()
plot(nci.som, type = "mapping", col = "black", bgcol = bgcolumns)
add.cluster.boundaries(nci.som, groups.3)




