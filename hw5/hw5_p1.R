library(bnlearn)
library(Rgraphviz)

titanic.data <- read.csv('titanic.csv')
names(titanic.data)
titanic.data <- titanic.data[,c("Survived","Pclass","Sex","Age","Siblings.Spouses.Aboard","Parents.Children.Aboard")]
names(titanic.data)
for(x in seq(1,3)){
  titanic.data[,x]<-as.factor(titanic.data[,x])
}
titanic.data$Age[titanic.data$Age >15] <- 'Adult'
titanic.data$Age[titanic.data$Age != 'Adult'] <- 'Children'
for(x in seq(4,6)){
titanic.data[,x]<-as.factor(titanic.data[,x])
}
titanic.data <- titanic.data[complete.cases(titanic.data), ]  # Rows are removed where NA are present
titanic.data
titanic.data <- data.frame(titanic.data)
titanic.dag <- hc(titanic.data)
x11()
plot(titanic.dag)
fitbn <- bn.fit(titanic.dag, data = titanic.data)
x11()
graphviz.plot(fitbn)
cpquery(fitbn, (Survived == 1), (Sex == 'female'))
cpquery(fitbn, (Survived == 1), (Sex == 'female'))
cpquery(fitbn, (Survived == 1), (Age =='Children'))  
cpquery(fitbn, (Survived == 1), (Age =='Adult'))
cpquery(fitbn, (Survived == 1), (Sex == 'female' & Pclass == 1))
cpquery(fitbn, (Survived == 1), (Sex == 'female' & Pclass == 3))
cpquery(fitbn, (Survived == 1), (Age == 'Children'  & Pclass == 3))
cpquery(fitbn, (Survived == 1), (Pclass == 3))
cpquery(fitbn, (Survived == 1), (Pclass == 1))
cpquery(fitbn, (Survived == 1), (Age == 'Children'  & Sex=='female' & Pclass == 1))
cpquery(fitbn, (Survived == 1), (Sex == 'female' & Age == 'Children'))
cpquery(fitbn, (Survived == 0), (Sex == 'male' & Pclass==3 & Age=='Adult'))
cpquery(fitbn, (Survived == 1), (Sex == 'female' & Pclass==1 & Age=='Adult'))
