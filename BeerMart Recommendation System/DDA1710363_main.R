
library(recommenderlab)
library(ggplot2)
library(dplyr)

beermart_df <- read.csv("C:\\nitin\\beer_data.csv", header=TRUE)


############# Distribution ##############

### review_profilename      review_overall 
#   northyorksammy:  1846   Min.   :0.000  
#   mikesgroove   :  1379   1st Qu.:3.500  
#   BuckeyeNation :  1338   Median :4.000  
#   Thorpe429     :  1072   Mean   :3.815  
#   ChainGangGuy  :  1046   3rd Qu.:4.500  
#   NeroFiddled   :  1031   Max.   :5.000  
#   (Other)       :468272  

# 6 top users who give maximum reviews:
#         northyorksammy, mikesgroove, BuckeyeNation
#         Thorpe429, ChainGangGuy, NeroFiddled

summary(beermart_df)
str(beermart_df)

# there are reviews without user name
# remove those rows without user name
beermart_df$review_profilename <- as.character(beermart_df$review_profilename)
beermart_df <- beermart_df %>% filter(beermart_df$review_profilename !="")

# unique reviews : 10
# 3.0 4.0 3.5 4.5 2.5 5.0 2.0 1.0 1.5 0.0
length(unique(beermart_df$review_overall))

# unique users : 22497
length(unique(beermart_df$review_profilename))

#unique beer : 40304
length(unique(beermart_df$beer_beerid))


# check for users who have rated same beer more than once
duplicated_rating <- beermart_df[duplicated(beermart_df[,-3]),]


# remove duplicate records of rating by same user for same beer
beermart_df <- beermart_df[!duplicated(beermart_df[,-3]),]

# reviews are skewed more towards right with median 4 and mean 3.8
ggplot(beermart_df,aes(x=review_overall))+geom_histogram()


# remove beer with zero ratings
beermart_df_rating <- beermart_df %>% filter(beermart_df$review_overall != 0)

# group by beerid 
beermart_df_beerid <- beermart_df_rating %>% group_by(beer_beerid) %>% summarise(No. = n())


# distribution of beer with no. of reviews available 
# 90% of beers have number of reviews less than 20.
quantile(beermart_df_beerid$No.,seq(0,1,.01))
boxplot(beermart_df_beerid$No.)

# identify beerid who have more than 40 reviews recorded.
review_df <- beermart_df_beerid %>% filter(beermart_df_beerid$No. > 20)

# collect beer id records with more than 20 reviews
# there are 4111 beers with more than 20 reviews
beermart_df_review <- beermart_df_rating %>% filter(beermart_df_rating$beer_beerid %in% review_df$beer_beerid)

beermart_df_review <- beermart_df_review[,c("review_profilename","beer_beerid","review_overall")]

# group by user 
beermart_df_user <- beermart_df_review %>% group_by(review_profilename) %>% summarise(No. = n())


quantile(beermart_df_user$No.,seq(0,1,.01))

# Min. 1st Qu.  Median    Mean    3rd Qu.    Max. 
# 1.00    1.00  3.00      17.49   11.00      821.00
# Mostly users have rated 3 movies
summary(beermart_df_user$No.)
boxplot(beermart_df_user$No.)


# identify users who have rated more than 3 movies
user_df <- beermart_df_user %>% filter(beermart_df_user$No. > 3)

# remove rows for users who have rated less than 3 movies
beermart_df_review_user <- beermart_df_review %>% filter(beermart_df_review$review_profilename %in% user_df$review_profilename)

# convert beermart_df_review data frame to a "realratingMatrix"
beermart_matrix <- as(beermart_df_review_user, "realRatingMatrix")


# coerce the matrix to a dataframe to understand user and item 
beermart_df_coerced <- as(beermart_matrix, "data.frame")
str(beermart_df_coerced)


#How similar are the first ten users are with each other

similar_users <- similarity(beermart_matrix[1:10,],
                            method = "cosine",
                            which = "users")

#Similarity matrix
as.matrix(similar_users)

#Visualise similarity matrix
image(as.matrix(similar_users), main = "User similarity")



#How similar are the first ten beer are with each other

similar_beer <- similarity(beermart_matrix[,1:10 ],
                            method = "cosine",
                            which = "items")

#Similarity matrix
as.matrix(similar_beer)

#Visualise similarity matrix
image(as.matrix(similar_beer), main = "Beer similarity")



# Visualizing ratings
qplot(getRatings(beermart_matrix), binwidth = 1, 
      main = "Histogram of ratings", xlab = "Rating")

#  Min.   1st Qu.  Median  Mean    3rd Qu.  Max. 
# 1.000   3.500    4.000   3.869   4.500    15.000 
summary(getRatings(beermart_matrix)) # Skewed to the right


# Since reviews are more skewed to right, normalize them.
qplot(getRatings(normalize(beermart_matrix, method = "Z-score")),
      main = "Histogram of normalized ratings", xlab = "Rating") 

summary(getRatings(normalize(beermart_matrix, method = "Z-score"))) # seems better


# distribution of reviews based on user
# Most users rate less number of movies.
# Very few users have rated more movies
qplot(rowCounts(beermart_matrix), binwidth = 10, 
      main = "Beer Ratings on User", 
      xlab = "# of users", 
      ylab = "# of beer rated")

# average number of ratings given by user: 17.5
#  Min.  1st Qu.  Median   Mean   3rd Qu.   Max. 
#  1.0   1.0      3.0      17.5   11.0      822.0 
summary(rowCounts(beermart_matrix))
  

# distribution of reviews based on item
# Less beer are rated more frequently by users
qplot(colCounts(beermart_matrix), binwidth = 10, 
      main = "Beer Ratings on beerid", 
      xlab = "# of beerid", 
      ylab = "# of beer rated")

# average number of ratings given to beer: 87.78
# Min.   1st Qu.  Median    Mean    3rd Qu.    Max. 
# 19.00  30.00    50.00     87.78   99.00      977.00
summary(colCounts(beermart_matrix))



# visualize average ratings by users
qplot(rowMeans(beermart_matrix))

# average rating by user: 3.932
# Min.   1st Qu.  Median  Mean    3rd Qu.   Max. 
# 1.000   3.682   4.000   3.932   4.250     5.000 
summary(rowMeans(beermart_matrix))


# visualize average ratings per beer
qplot(colMeans(beermart_matrix))

# average ratinge per beer: 3.776
# Min.   1st Qu.  Median    Mean    3rd Qu.    Max. 
# 1.426  3.618    3.829     3.776   4.013      4.667 
summary(colMeans(beermart_matrix))


#Visualizing the matrix:
image(beermart_matrix, main = "Heatmap of the rating matrix")
image(beermart_matrix[1:50,1:50], main = "Heatmap of the rating matrix for first 50 users on first 50 beers")

############## Recommendation Models ################


#Divide data into test 
scheme <- evaluationScheme(beermart_matrix, method = "split", train = .9,
                           k = 1, given = 3, goodRating = 4)

#--arguments
#train and test
#Here we create an evaluation scheme which splits the users 
#into a training set (90%) and a test set (10%). 

#given 
#For the test set 3 items(given argument) will be given to the
#recommender algorithm and the other items will be held out for computing the error

#With goodRating=4 all items with actual user rating of greater or equal 4 are considered 
#positives in the evaluation process


algorithms <- list(
  "user-based CF" = list(name="UBCF", param=list(normalize = "Z-score",
                                                 method="Cosine",
                                                 nn=30, minRating=3)),
  "item-based CF" = list(name="IBCF", param=list(normalize = "Z-score"
  ))
)


# run algorithms, predict next topn beers
results <- evaluate(scheme, algorithms, n=c(1, 3, 5, 10, 15, 20))
avg(results)

# Draw ROC curve
# UBCF gives better results
plot(results, annotate = 1:4, legend="topleft")

# See precision / recall
plot(results, "prec/rec", annotate=3)


#use evaluatescheme with 4 fold cross validation 
scheme_cv <- evaluationScheme(beermart_matrix, method = "cross-validation", 
                           k = 4, given = 3, goodRating = 4)


#using only UBCF evaluation
algorithms_cv <- list(
  "user-based CF" = list(name="UBCF", param=list(normalize = "Z-score",
                                                 method="Cosine",
                                                 nn=30, minRating=3)),
  "item-based CF" = list(name="IBCF", param=list(normalize = "Z-score"
  ))
  )


# run algorithms, predict next topn beers
results_cv <- evaluate(scheme_cv, algorithms_cv, n=c(1, 3, 5, 10, 15, 20))
avg(results_cv)

# Draw ROC curve
plot(results_cv, annotate = 1:4, legend="topleft")

# See precision / recall
plot(results_cv, "prec/rec", annotate=3)


############### Predictions ################3

# Find indices of user cokes, genog and giblet

# 1938
which(beermart_matrix@data@Dimnames[[1]] == "cokes")

# 3234
which(beermart_matrix@data@Dimnames[[1]] == "genog")

# 3278
which(beermart_matrix@data@Dimnames[[1]] == "giblet")

# find top 5 beer recommendations for users using UBCF and split train-test
rec <- Recommender(getData(scheme, "train"), "UBCF")
pred <- predict(rec, beermart_matrix[c(1938,3234,3278),], type="topNList", n=5)



# cokes :  "6746"  "226"   "3916"  "22505" "1276" 
# genog :  "35732" "155"   "35738" "276"   "7971" 
# giblet : "39"    "1904"  "1717"  "15881" "7971" 
as(pred,"list")



# find top 5 beer recommendations for users using UBCF and 4 fold cross validation
rec_cv <- Recommender(getData(scheme_cv, "train"), "UBCF")
pred_cv <- predict(rec_cv, beermart_matrix[c(1938,3234,3278),], type="topNList", n=5)



# cokes :  "6746"  "1339"  "16814" "1371"  "226" 
# genog :  "155"   "7971"  "34420" "6076"  "57908"
# giblet : "39"    "141"   "1717"  "15881" "34420" 
as(pred_cv,"list")


####################### Solution ##########################

#### EDA ####

#  only those beers that have more than 20 number of reviews were selected.
#  those users who had voted for more than 3 beers were selected.

# unique values of rating in original set:

# The average beer ratings : 3.776

# The average user ratings : 3.932

# The average number of ratings given to the beers : 87.78

# The average number of ratings given by the users : 17.5


#### Recommendation Model ####

# Evaluation scheme used with split and 4 fold cross-validation approach. 
# IBCF and UBCF used for evaluation, UBCF was found better looking at ROC curve.

# Top 5 beers rcommended for users:
# cokes :  "6746"  "226"   "3916"  "22505" "1276" 
# genog :  "35732" "155"   "35738" "276"   "7971" 
# giblet : "39"    "1904"  "1717"  "15881" "7971" 











