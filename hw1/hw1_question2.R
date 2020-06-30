rm(list = ls())
graphics.off()

library("arules")
library(MASS)
########################
## 
##    Question 2 
########################
data(Boston)
summary(Boston)

boston_List <- as(Boston,"list")
boston_Bin <-  as(boston_List,"transactions")

x11()
itemFrequencyPlot(boston_Bin,support = 0.2, cex.names = 0.8)

rules <- apriori(boston_List,parameter = list(support = 0.2, confidence = 0.1))
summary(rules)

Boston[["zn"]] <- NULL
Boston[["indus"]] <- NULL
Boston[["chas"]] <- NULL
Boston[["rm"]] <- NULL
Boston[["age"]] <- NULL
Boston[["rad"]] <- NULL
Boston[["tax"]] <- NULL
Boston[["ptratio"]] <- NULL
Boston[["black"]] <- NULL
Boston[["lstat"]] <- NULL
Boston[["medv"]] <- NULL
Boston[["nox"]] <- NULL

#AdultUCI[["age"]] <- ordered(cut(AdultUCI[["age"]], c(15, 25, 45, 65, 100)), labels = c("Young", "Middle-aged", "Senior", "Elderly"))
Boss <- as(Boston, "transactions")
summary(Boss)

x11()
itemFrequencyPlot(Boss, support = 0.05, cex.names = 0.8)

# Apply the apriori algorithm
rules  <- apriori(Boss, parameter = list(support = 0.001, confidence = 0.6))

# Take a closer look at the different rules
summary(rules)

setwd("C:/Users/Shreyas/Documents")
write(rules, file = "large_rules.csv", sep = ",", col.names = NA)


#########################

data("Boston")
linearMod <- lm(dist ~ speed, data=Boston)

