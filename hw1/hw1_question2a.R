library(MASS)
library(arules)

data("Boston")
head(Boston)
summary(Boston)
pairs(Boston)

#A
x11()
hist(Boston$medv, xlab="MEDV" ,col="darkmagenta")
hist(Boston$crim ,xlab="CRIM" ,col="darkmagenta")
hist(Boston$zn, xlab="ZN" ,col="darkmagenta")
hist(Boston$indus, xlab="INDUS" ,col="darkmagenta")
hist(Boston$chas, xlab="CHAS" ,col="darkmagenta")
hist(Boston$nox, xlab="NOX" ,col="darkmagenta")
hist(Boston$rm, xlab="RM" ,col="darkmagenta")
hist(Boston$age, xlab="AGE" ,col="darkmagenta")
hist(Boston$dis, xlab="DIS" ,col="darkmagenta")
hist(Boston$rad, xlab="RAD" ,col="darkmagenta")
hist(Boston$tax, xlab="TAX" ,col="darkmagenta")
hist(Boston$ptratio, xlab="PTRATIO" ,col="darkmagenta")
hist(Boston$black, xlab="BLACK" ,col="darkmagenta")
hist(Boston$lstat, xlab="LSTAT" ,col="darkmagenta")

Boston[["crim"]] <- ordered(cut(Boston[["crim"]], c(0,0.1,3.61,5,90)), labels = c('verylowcrime','lowcrime','moderatecrime','highcrime'))
Boston[["zn"]] <- ordered(cut(Boston[["zn"]], c(0,11,12,20,100)), labels = c('nonresidential','smallresidence','moderateresidence','largeresidence'))
Boston[["indus"]] <- ordered(cut(Boston[["indus"]], c(0,5,9,20,30)), labels = c('verylowbusinessconcentration','lowbusinessconcentration','moderatebusinessconcentration','highbusinessconcentration'))
Boston[["chas"]] <- NULL
Boston[["nox"]] <- NULL
Boston[["rm"]] <- ordered(cut(Boston[["rm"]], c(1,5,7,10)), labels = c('smallhouse','largehouse','verylargehouse'))
Boston[["age"]] <- ordered(cut(Boston[["age"]], c(0,20,49,100)), labels = c('fewoldhouses','oldhousesinmoderation','lotsofoldhouses'))
Boston[["dis"]] <- ordered(cut(Boston[["dis"]], c(0,2,5,12,15)), labels = c('verynear','near','far','veryfar'))
Boston[["rad"]] <- ordered(cut(Boston[["rad"]], c(0,5,8,25)), labels = c('easilyaccessible','moderatelyaccessible','notaccessible'))
Boston[["tax"]] <- ordered(cut(Boston[["tax"]], c(0,250,450,1000)), labels = c('lowtax','moderatetax','hightax'))
Boston[["ptratio"]] <- ordered(cut(Boston[["ptratio"]], c(10,15,17,25)), labels = c('smallclass','mediamsizedclass','largeclass'))
Boston[["black"]] <- NULL
Boston[["medv"]] <- ordered(cut(Boston[["medv"]], c(1,20,35,60)), labels = c('cheap','moderate','expensive'))
Boston[["lstat"]] <- NULL
summary(Boston)

boston_tr <- as(Boston,'transactions')

#B

itemFrequencyPlot(boston_tr,support = 0.05, cex.names = 0.8 )

rules <- apriori(boston_tr, parameter = list(support = 0.02, confidence = 0.8))
summary(rules)

# C
rules_CloseToCity <- subset(rules, subset = rhs %in% "dis=verynear" & lift >1.2)
rules_LowCrime <- subset(rules, subset = rhs %in% "crim=verylowcrime" & lift >1.2)
summary(rules_CloseToCity)
summary(rules_LowCrime)

rulesLowCrimeNearCity <- subset(rules, subset = rhs %in% "crim=lowcrime" & lhs %in% "dis=verynear" & lift >1.2)
summary(rulesLowCrimeNearCity)


# D
rulesLowPupil_TeacherRatio <- subset(rules, subset = rhs %in% "ptratio=smallclass" & lift >1.2)

summary(rulesLowPupil_TeacherRatio)
inspect(head(sort(rulesLowPupil_TeacherRatio, by ='lift'),n = 6))

#Regression Model
subset = data.frame(lapply(subset, function(x) as.numeric(as.character(x))))
modela = lm(rulesLowPupil_TeacherRatio, subset)
summary(modela)

setwd("C:/Users/Shreyas/Documents")
write(rulesLowCrimeNearCity, file='./Question2.csv',sep = ",", col.names = NA )
