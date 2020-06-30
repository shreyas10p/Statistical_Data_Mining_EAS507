library(ElemStatLearn)
rm(list = ls(all = T))
data("marketing")
#load(marketing.RData)
head(marketing)

refData = sample(marketing)
marketing$flag<-1
refData$flag<-0

rbind(marketing, referenceData, flag)
mergeData <- merge(marketing,referenceData,by="flag")

library(rpart)
model = rpart(flag~., mergeData)
summary(model)
prediction = predict(model, mergedata[,-c(15)])
prediction

