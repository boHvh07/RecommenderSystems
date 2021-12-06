library(recommenderlab)

### DAta ###
data("MovieLense")

MovieLense

getRatingMatrix(MovieLense)[1:10,1:5]

as(MovieLense, "matrix")[1:10, 1:5]

test<-as(MovieLense, "matrix")[1:10,]

image(MovieLense)

### Content Filtering ###
head(MovieLenseMeta)

item<-as.matrix(subset(MovieLenseMeta, select = -c(title, year, url)))

rating<-as(MovieLense, "matrix")[1,]

rating_m<-rating-mean(rating,na.rm=TRUE)

non_miss<-!is.na(rating_m)
miss<-is.na(rating_m)

user<-(t(item[non_miss,]) %*% rating_m[non_miss]) / colSums(item[non_miss, ])

user

names<-as.matrix(subset(MovieLenseMeta, select = c(title)))

new_item<-item[miss,]
new_names<-names[miss,]

CS = (new_item) %*% user / (sqrt(rowSums(new_item^2))*sqrt(sum(user^2)))

hist(CS, main = "histogram of cosine similarity with unseen movies", xlab="Cosine Similarity")

new_names[head(order(CS, decreasing = TRUE))]

### Popularity Rec ###
row_names<-c("A", "B", "C", "D", "E", "F", "G")
col_names<-c("PW", "TR", "EB", "T2", "P", "NH")

util <- matrix(c(2,5,4,2,NA, NA,
                 5,1,2,NA,1,NA,
                 5,5,5,5,5,5,
                 2,5,NA,3,NA,NA,
                 5,4,5,3,NA,5,
                 1,5,NA,NA,NA,1,
                 2,NA,5,NA,5,NA),byrow = TRUE, nrow = 7, ncol = 6, dimnames=list(row_names,col_names))
util

util_n <-util-rowMeans(util, na.rm=TRUE)

colMeans(util_n,na.rm = TRUE)

test<- as(util, "realRatingMatrix")
test_recom<-Recommender(test, method = "POPULAR")
test_recom@model$topN@items

test_pred<-predict(test_recom, test[1,],type="ratings")

as(test_pred,"matrix")

### MovieLense ###
set.seed(19103)
es <- evaluationScheme(MovieLense, 
                       method="split", train=0.9, given=15)

es

train <- getData(es, "train"); train

test_known <- getData(es, "known"); test_known

test_unknown <- getData(es, "unknown"); test_unknown

popular <-Recommender(train, "POPULAR")

## create predictions for the test users using known ratings
pred_pop <- predict(popular, test_known, type="ratings"); pred_pop

## evaluate recommendations on "unknown" ratings
acc_pop <- calcPredictionAccuracy(pred_pop, test_unknown);
as(acc_pop,"matrix")

as(test_unknown, "matrix")[1:8,1:5]

as(pred_pop, "matrix")[1:8,1:5]


### User-Based Collaborative Filtering ###
UBCF<-Recommender(train, "UBCF",
                  param=list(method="pearson",nn=30))

## create predictions for the test users using known ratings
pred_ub <- predict(UBCF, test_known, type="ratings"); pred_ub

## evaluate recommendations on "unknown" ratings
acc_ub <- calcPredictionAccuracy(pred_ub, test_unknown);
acc<-rbind(POP=acc_pop, UBCF = acc_ub); acc

as(test_unknown, "matrix")[1:8,1:5]

as(pred_ub, "matrix")[1:8,1:5]

### Item-Based Collaborative Filtering ###
IBCF <- Recommender(train, "IBCF",
                    param=list(method="pearson",k=30))
pred_ib <- predict(IBCF, test_known, type="ratings")
acc_ib <- calcPredictionAccuracy(pred_ib, test_unknown) 
acc <- rbind(POP=acc_pop, UBCF = acc_ub, IBCF = acc_ib); acc

### Matrix Decomposition ###
MAT<- Recommender(train, "SVDF")
pred_mat <- predict(MAT, test_known, type="ratings")
acc_mat <- calcPredictionAccuracy(pred_mat, test_unknown) 
acc <- rbind(POP=acc_pop, UBCF = acc_ub, IBCF = acc_ib, MAT=acc_mat); acc

movie_factors<-MAT@model$svd$V
user_factors<-MAT@model$svd$U

some_movie_id = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 122, 365, 121, 405, 311, 117, 22, 299, 281, 1318, 261, 404, 21, 35)

labels<-as.character(MovieLenseMeta[,1])
plot(movie_factors[,1], movie_factors[,2], xlab="Movie factor 1", ylab="Movie factor 2", col="lightblue", pch=19, cex=2)
text(movie_factors[some_movie_id,1], movie_factors[some_movie_id,2], labels = labels[some_movie_id], cex=0.5, font=2)