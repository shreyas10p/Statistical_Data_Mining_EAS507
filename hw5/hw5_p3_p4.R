library(recommenderlab)
data(MovieLense)
MovieLense
head(MovieLense)

dim(MovieLense)
MovieLense
head(as(MovieLense[1,], "list")[[1]])   # Movies rated by 1st user
getRatingMatrix(MovieLense)[1:10,1:10]
dim(getRatingMatrix(MovieLense))
x11()
image(MovieLense[1:100,1:100])
#---------------------------------------Question 3---------------------------------------

#Normalise Data
R_Normalize <- normalize(MovieLense)
R_Normalize
x11()
image(R_Normalize[1:100,1:100])

x11()
hist(getRatings(R_Normalize), breaks = 100, main = "Histogram of normalized ratings")

x11()
hist(rowCounts(R_Normalize), breaks = 100, main = "ratings given by users")

x11()
hist(colCounts(R_Normalize), breaks = 100, main = "count of ratings per movie")

#data preparation
eval <- evaluationScheme(R_Normalize, method = "split", given = 3, train = 0.75, goodRating = 4)
eval
userbased_model<- Recommender(getData(eval,"train"), "UBCF", param = list(method = "cosine",nn = 50)) #User based collaborative filtering
userbased_model
user_rating <- predict(userbased_model, getData(eval, "known"), type = "ratings")   #Make predictions on ratings
user_rating
as(user_rating, "matrix")[1:10,1:10]
getRatingMatrix(user_rating)[1:5,1:5]
pred_error <- rbind(UBCF = calcPredictionAccuracy(user_rating, getData(eval, "unknown")))  #Calculate error for question 3
pred_error


###############-------------------------------Question 4-----------------------

#Cross Validation K=5 
set.seed(2018)
scheme_1 <- evaluationScheme(MovieLense, method = "cross", k = 5, given = 3, goodRating = 4)
scheme_1

userbased_model_4 <- Recommender(getData(scheme_1, "train"), "UBCF", param = list(method = "cosine",nn = 50))
userbased_model_4
pred_rating_4 <- predict(userbased_model_4, getData(scheme_1, "known"), type = "ratings")
as(pred_rating_4,"matrix")[1:10,1:2]
getRatingMatrix(pred_rating_4)[1:5,1:5]
#calculate error for Question 4
pred_error_4 <- rbind(UBCF = calcPredictionAccuracy(pred_rating_4, getData(scheme_1, "unknown")))
pred_error_4
results_4 <- evaluate(scheme_1, method = "UBCF", n=c(1,3,5,10,15,20), param = list(method = "cosine"))
avg(results_4)


results_3 <- evaluate(x = eval, method = "UBCF", param = list(method = "cosine"), type = "ratings")
avg(results_3)


x11()
plot(results_3, annotate = TRUE)
title("Error histogram")


x11()
plot(results_4, annotate = TRUE)
title("ROC Curve")

x11()
plot(results_4, "prec/rec", annotate = TRUE)
title("Precision-recall")