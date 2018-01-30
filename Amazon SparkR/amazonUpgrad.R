
# 1. Initialize spark session
# load SparkR
library(SparkR)

# initiating the spark session
sc = sparkR.session(master='local')

# 2. Create a Spark DataFrame and examine structure
# reading CD and Vinyl json data from S3 bucket
reviews_CDs_and_Vinyl <- SparkR::read.df("s3a://amazonupgrad/reviews_CDs_and_Vinyl_5.json", source="json")
printSchema(reviews_CDs_and_Vinyl)
head(reviews_CDs_and_Vinyl)

# count number of reviews on CDs and Vinyl category
# 1097592
nrow(reviews_CDs_and_Vinyl)

# Distinct reviewID for CDs and Vinyl category: 75258
collect(select(reviews_CDs_and_Vinyl, countDistinct(reviews_CDs_and_Vinyl$reviewerID)))

# check if reviewerID contains NULL: No null
reviews_CDs_and_Vinyl_Null <- where(reviews_CDs_and_Vinyl, isNull(reviews_CDs_and_Vinyl$reviewerID))
count(reviews_CDs_and_Vinyl_Null)

# create temporary table 
createOrReplaceTempView(reviews_CDs_and_Vinyl, "reviews_CDs_and_Vinyl")

# recheck number of rows: 1097592
number_reviews_CDs_and_Vinyl <- collect(sql("select count(*) from reviews_CDs_and_Vinyl"))


# Create a Spark DataFrame and examine structure
# reading CD and Vinyl json data from S3 bucket
reviews_Kindle_Store <- SparkR::read.df("s3a://amazonupgrad/reviews_Kindle_Store_5.json", source="json")
head(reviews_Kindle_Store)

# count number of reviews on Kindle category
# 982619
nrow(reviews_Kindle_Store)

# Distinct reviewID for CDs and Vinyl category: 68223
collect(select(reviews_Kindle_Store, countDistinct(reviews_Kindle_Store$reviewerID)))

# check if reviewerID contains NULL: No null
reviews_Kindle_Store_Null <- where(reviews_Kindle_Store, isNull(reviews_Kindle_Store$reviewerID))
count(reviews_Kindle_Store_Null)

# create temporary table 
createOrReplaceTempView(reviews_Kindle_Store, "reviews_Kindle_Store")

# recheck number of rows: 982619
number_reviews_Kindle_Store <- collect(sql("select count(*) from reviews_Kindle_Store"))


# Create a Spark DataFrame and examine structure
# reading Movies_and_TV json data from S3 bucket
reviews_Movies_and_TV <- SparkR::read.df("s3a://amazonupgrad/reviews_Movies_and_TV_5.json", source="json")
head(reviews_Movies_and_TV)

# count number of reviews on Movie and TV category
# 1697533
nrow(reviews_Movies_and_TV)

# Distinct reviewID for Movie and TV category: 123960
collect(select(reviews_Movies_and_TV, countDistinct(reviews_Movies_and_TV$reviewerID)))


# check if reviewerID contains NULL: No null
reviews_Movies_and_TV_Null <- where(reviews_Movies_and_TV, isNull(reviews_Movies_and_TV$reviewerID))
count(reviews_Movies_and_TV_Null)

# create temporary table 
createOrReplaceTempView(reviews_Movies_and_TV, "reviews_Movies_and_TV")

# recheck number of rows: 1697533
number_reviews_Movies_and_TV <- collect(sql("select count(*) from reviews_Movies_and_TV"))


########################## Heavily Purchased Product Categroy #######################


# Use number of reviews for each category to directly infer number of products purchased.

# CD and Vinyl:   1097592
# Kindle:         982619
# Movies and TV:  1697533

# Movies and TV category has highest number of products purchased, followed by
# CD and Vinyl.


########################## Heavily Purchased Product Categroy #######################




########################## Market Share - Product Categroy #########################

# Percentage of products reviewed per category of total reviews for all three categories

# CD and Vinyl:   28.29%
# Kindle:         25.24%
# Movies and TV:  43.60%

# Percentage of reviews for a category of total reviews of category can be inferred to give percentage share

# Movies and TV has highest market share.


total_market_categories_3 <- number_reviews_CDs_and_Vinyl + number_reviews_CDs_and_Vinyl + number_reviews_Movies_and_TV
# 3892717
total_market_categories_3 


CDs_and_Vinyl_share <- (number_reviews_CDs_and_Vinyl/total_market_categories_3)*100
# 28.19%
CDs_and_Vinyl_share

kindle_share <- (number_reviews_Kindle_Store/total_market_categories_3)*100
# 25.24%
kindle_share

Movies_and_TV_share <- (number_reviews_Movies_and_TV/total_market_categories_3)*100
# 43.60%
Movies_and_TV_share


########################## Market Share - Product Categroy #########################

# Create a new attribute that should consider effect of overall rating and helful score
# new 'overallScore' defined whose value is product of helpful score and overall rating

reviews_CDs_and_Vinyl_helpfulScore <- collect(select(reviews_CDs_and_Vinyl,reviews_CDs_and_Vinyl$overall, reviews_CDs_and_Vinyl$helpful))
reviews_CDs_and_Vinyl_helpfulScore$helfulTags <- sapply(reviews_CDs_and_Vinyl_helpfulScore$helpful, "[[", 1)
reviews_CDs_and_Vinyl_helpfulScore$totalTags <- sapply(reviews_CDs_and_Vinyl_helpfulScore$helpful, "[[", 2)
reviews_CDs_and_Vinyl_helpfulScore$score <- reviews_CDs_and_Vinyl_helpfulScore$helfulTags/reviews_CDs_and_Vinyl_helpfulScore$totalTags

# summary of category on overall and score
# There are no null values for 'overall' column
# There are null or NaN for score column implying some reviews have no available data
# 'helpful' column has some invalid dataset - where helpful tags are more than total tags eg.,(3,1)
summary(reviews_CDs_and_Vinyl_helpfulScore)

# remove these rows with invalid dataset for helpful column
reviews_CDs_and_Vinyl_helpfulScore <- reviews_CDs_and_Vinyl_helpfulScore[which(reviews_CDs_and_Vinyl_helpfulScore$score <= 1),]


# replace NaN score values with median score values
reviews_CDs_and_Vinyl_helpfulScore$score[which(is.na(reviews_CDs_and_Vinyl_helpfulScore$score))] <- median(reviews_CDs_and_Vinyl_helpfulScore$score[-which(is.na(reviews_CDs_and_Vinyl_helpfulScore$score))])


# compute new attribute 'overallScore' which consider score and rating of review for an entry
reviews_CDs_and_Vinyl_helpfulScore$overallScore <- reviews_CDs_and_Vinyl_helpfulScore$overall * reviews_CDs_and_Vinyl_helpfulScore$score

# average of 'overallScore' for CD and Vinyl category
avg_overallScore_CD_Vinyl <- mean(reviews_CDs_and_Vinyl_helpfulScore$overallScore)

# 3.30
avg_overallScore_CD_Vinyl



reviews_Kindle_Store_helpfulScore <- collect(select(reviews_Kindle_Store,reviews_Kindle_Store$overall, reviews_Kindle_Store$helpful))
reviews_Kindle_Store_helpfulScore$helfulTags <- sapply(reviews_Kindle_Store_helpfulScore$helpful, "[[", 1)
reviews_Kindle_Store_helpfulScore$totalTags <- sapply(reviews_Kindle_Store_helpfulScore$helpful, "[[", 2)
reviews_Kindle_Store_helpfulScore$score <- reviews_Kindle_Store_helpfulScore$helfulTags/reviews_Kindle_Store_helpfulScore$totalTags

# summary of category on overall and score
# There are no null values for 'overall' column
# There are null or NaN for score column implying some reviews have no available data
summary(reviews_Kindle_Store_helpfulScore)


# replace NaN score values with median score values
reviews_Kindle_Store_helpfulScore$score[which(is.na(reviews_Kindle_Store_helpfulScore$score))] <- median(reviews_Kindle_Store_helpfulScore$score[-which(is.na(reviews_Kindle_Store_helpfulScore$score))])


# compute new attribute 'overallScore' which consider score and rating of review for an entry
reviews_Kindle_Store_helpfulScore$overallScore <- reviews_Kindle_Store_helpfulScore$overall * reviews_Kindle_Store_helpfulScore$score

# average of 'overallScore' for Kindle category
avg_overallScore_Kindle <- mean(reviews_Kindle_Store_helpfulScore$overallScore)

# 4.02
avg_overallScore_Kindle



reviews_Movies_and_TV_helpfulScore <- collect(select(reviews_Movies_and_TV,reviews_Movies_and_TV$overall, reviews_Movies_and_TV$helpful))
reviews_Movies_and_TV_helpfulScore$helfulTags <- sapply(reviews_Movies_and_TV_helpfulScore$helpful, "[[", 1)
reviews_Movies_and_TV_helpfulScore$totalTags <- sapply(reviews_Movies_and_TV_helpfulScore$helpful, "[[", 2)
reviews_Movies_and_TV_helpfulScore$score <- reviews_Movies_and_TV_helpfulScore$helfulTags/reviews_Movies_and_TV_helpfulScore$totalTags

# summary of category on overall and score
# There are no null values for 'overall' column
# There are null or NaN for score column implying some reviews have no available data
# 'helpful' column has some invalid dataset - where helpful tags are more than total tags eg.,(3,1)
summary(reviews_Movies_and_TV_helpfulScore)

# remove these rows with invalid dataset for helpful column
reviews_Movies_and_TV_helpfulScore <- reviews_Movies_and_TV_helpfulScore[which(reviews_Movies_and_TV_helpfulScore$score <= 1),]

# replace NaN score values with median score values
reviews_Movies_and_TV_helpfulScore$score[which(is.na(reviews_Movies_and_TV_helpfulScore$score))] <- median(reviews_Movies_and_TV_helpfulScore$score[-which(is.na(reviews_Movies_and_TV_helpfulScore$score))])


# compute new attribute 'overallScore' which consider score and rating of review for an entry
reviews_Movies_and_TV_helpfulScore$overallScore <- reviews_Movies_and_TV_helpfulScore$overall * reviews_Movies_and_TV_helpfulScore$score

# average of 'overallScore' for Movies and TV category
avg_overallScore_Movies_TV <- mean(reviews_Movies_and_TV_helpfulScore$overallScore)

# 2.58
avg_overallScore_Movies_TV



############################# Overall Score per Category #########################

avg_overallScore_CD_Vinyl
avg_overallScore_Kindle
avg_overallScore_Movies_TV

# CD and Vinyl:   3.30
# Kindle      :   4.02
# Movies and TV:  2.58 

# Kindle Reviews and ratings will make customers more happy


############################# Overall Score per Category #########################



############################# Product Rating ###########################


# Kindle Product Categoey has high average rating


############################# Product Rating ###########################

# estimate mean value of rating for CD and Vinyl category 
# 4.29
head(select(reviews_CDs_and_Vinyl,mean(reviews_CDs_and_Vinyl$overall)))


# estimate mean value of rating for Kindle category 
# 4.34
head(select(reviews_Kindle_Store,mean(reviews_Kindle_Store$overall)))


# estimate mean value of rating for Movies and TV category 
# 4.11
head(select(reviews_Movies_and_TV,mean(reviews_Movies_and_TV$overall)))



############################## Solution ###############################3

# 1.  Which product category has a larger market size
# Movies and TV


# 2.  Which product category is likely to be purchased heavily
# Movies and TV


# 3.  Which product category is likely to make the customers happy after the purchase
# Kindle Store
