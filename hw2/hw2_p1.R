summary(USArrests)
usa = USArrests



## ----------Question 1 (a)--------------------------------------------------------------

usa.dist = dist(usa)
usa.hclust = hclust(usa.dist)  #default method is complete linkage
plot(usa.hclust,labels=rownames(usa),main='Dendogram Question1',col="black")

## ----------Question 1 (b)--------------------------------------------------------------

groups.3=cutree(usa.hclust,3)

groups.3
table(groups.3)
rownames(usa)[groups.3==3]

## ----------Question 1 (c)--------------------------------------------------------------

usa.std = scale(usa)
usa.scdist = dist(usa.std)
usa.schclust = hclust(usa.scdist)
plot(usa.schclust,labels=rownames(usa),main='dendogram question1c',col="black")

## ----------Question 1 (d)--------------------------------------------------------------
scgroups.3=cutree(usa.schclust,3)

scgroups.3
table(scgroups.3)
table(groups.3,scgroups.3)

