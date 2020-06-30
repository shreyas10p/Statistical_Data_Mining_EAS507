library(ISLR)

library(kohonen)
usa = USArrests
head(USArrests)

#-----------------Question 2(a)--------------------------
#Scaling the data
usa.std <- scale(usa)

#calculating the distances
usa.dist = dist(usa.std)

#Hierarchical clustering
usa.hclust = hclust(usa.dist)  #default method is complete linkage
x11()
plot(usa.hclust,labels=rownames(usa),main='Dendogram Question1',col="black")

groups.3=cutree(usa.hclust,3)

rownames(usa)[groups.3==3]
table(groups.3)

#---------------------------Question 2(b)--------------------
set.seed(7)

#SOM algorithm
som_grid <- somgrid(xdim = 7, ydim = 7, topo = "hexagonal")
usa.som <- som(usa.std, grid = som_grid, rlen = 4000)

#SOM plots
x11()
plot(usa.som, main = "USA Data")


usa.som$unit.classif

x11()
plot(usa.som,type="changes", main = "USA Data")

x11()
plot(usa.som,type="count")

colorPalette <- function(n, alpha = 1){rainbow(n, end=7/10, alpha = alpha)[n:1]}

#U-Matrix Visualisation
x11()
plot(usa.som, type = "dist.neighbours",palette.name = colorPalette) 

#Map for Rape stats
x11()
plot(usa.som, type = "property", property = getCodes(usa.som)[,4], main=colnames(getCodes(usa.som))[4], palette.name=colorPalette)
#Map for Urban Pop stats
x11()
plot(usa.som, type = "property", property = getCodes(usa.som)[,3], main=colnames(getCodes(usa.som))[3], palette.name=colorPalette)
#Map for Assault stats
x11()
plot(usa.som, type = "property", property = getCodes(usa.som)[,2], main=colnames(getCodes(usa.som))[2], palette.name=colorPalette)

#Map for Murder stats
x11()
plot(usa.som, type = "property", property = getCodes(usa.som)[,1], main=colnames(getCodes(usa.som))[1], palette.name=colorPalette)

usacodes<-usa.som$codes[[1]]

distance <- dist(usacodes)
usa.hclust <- hclust(distance)
x11()
plot(usa.hclust,main='Dendogram USArrest',col="black")

groupsom.3<-cutree(usa.hclust,3)

rownames(usa)[groupsom.3==3]
table(groupsom.3)



