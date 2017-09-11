library(gdata)
library(dplyr)
library(stringr)
library(tidyr)

# Load the companies and rounds data into two data frames 
# and name them companies and rounds2 respectively.
comp <- read.delim("companies.txt",header=TRUE,fill=TRUE,stringsAsFactors = F)
rounds <- read.csv("rounds2.csv",header=TRUE,stringsAsFactors = F)
categ_mapping <- read.csv("mapping.csv",header=TRUE,stringsAsFactors = F)
eng_mapping <- read.csv("eng_countries.csv",header=TRUE,stringsAsFactors = F)


#===================CHECKPOIINT 1 - START========================

#=====================DATA CURATION=======================


# create working dataframes with distinct value
#df_comp <-  distinct(comp,permalink,.keep_all = TRUE)
#df_rounds <- distinct(rounds,company_permalink,.keep_all = TRUE)


# remove similar case duplicates
unique_rounds <- rounds[!duplicated(tolower(rounds$company_permalink)),]
unique_comp <- comp[!duplicated(tolower(comp$permalink)),]


# no. of unique companies in rounds and companies file
nrows_unique_rounds <- nrow(unique_rounds)
nrows_unique_comp <- nrow(unique_comp)


temp_comp <- comp
temp_rounds <- rounds

# convert to lower case all values in rounds and company dataframe 
# for permalink nd company_permalink columns for matching
temp_comp$permalink <- tolower(temp_comp$permalink) 
temp_rounds$company_permalink <- tolower(temp_rounds$company_permalink) 



# merge frames 
master_frame <- merge(temp_rounds, temp_comp, by.x = "company_permalink", by.y = "permalink", all.x=TRUE)

# comp frame has name column for companies
# check name column in merged frame to see if it is NA.
# this will tell if a startup is in rounds but not in company file.

find_missing_comp <- filter(master_frame,is.na(merged_frame$name == TRUE))
nrows_missing_comp <- nrows(find_missing_comp)



# merge frames

#master_frame <- merge(unique_rounds, unique_comp, by.x = "company_permalink", by.y = "permalink")


# no of rows in masterframe

nrows_master_frame <- nrow(master_frame)


#===================CHECKPOIINT 1 - END==========================


#===================CHECKPOIINT 2 - START========================


# no. of rows with NA values in raised_amount_usd column
nrows_NA_raisedAmount <- length(which(is.na(master_frame$raised_amount_usd)))

# replace 'NA' with zeros in raised_amount_usd column for rounds table
master_frame$raised_amount_usd[is.na(master_frame$raised_amount_usd)] <- 0

#===================CHECKPOIINT 2 - END==========================


#===================CHECKPOINT 3 - START=========================

# average funding amount raised by type of all fundings
avg_fundingAmt_byType <- master_frame %>% group_by(funding_round_type) %>% summarise(avg = mean(raised_amount_usd))


# average funding amount dataframe for venture, seed, angel, private_equity
avg_funding <- filter(avg_fundingAmt_byType,funding_round_type == "venture" | funding_round_type == "angel" |
                      funding_round_type == "seed" | funding_round_type == "private_equity")


# average funding amount for amoumnt betweem 5 million to 15 million USD
avg_funding_spark <- filter(avg_funding,avg_funding$avg > 5000000 & avg_funding$avg < 15000000)


# best funding type having maximum average amount of investement
max_fundAmount_byTpe <- avg_funding_spark[which.max(avg_funding_spark$avg),]


#==================CHECKPOINT 3 - END==============================


#==================CHECKPOINT 4 - START============================

# filter master frame for funding round type 'venture' and 
# remove rows where no country_code is specified.
angel_frame <- filter(master_frame,funding_round_type == max_fundAmount_byTpe$funding_round_type & !country_code == "")


# aggregate by total amount invested for each country 
angel_groupByCountry <- angel_frame %>% group_by(country_code) %>%  summarise(total = sum(raised_amount_usd)) 


# find top 9 countries
top9 <- angel_groupByCountry %>% top_n(9,total) 

top9_eng <- merge(top9,eng_mapping,by="country_code")
top9_eng <- filter(top9_eng,official_eng==TRUE)

top3 <- top9_eng %>% top_n(3,total)


#=====================CheckPoint 4- END=========================


#======================CheckPoint 5 - START======================

category_masterframe <- master_frame

# business rule: first string before the vertical bar 
# will be considered the primary sector.

category_masterframe$category_list <- sapply(category_masterframe$category_list,
                                             function(x) str_split(x,'\\|',simplify = TRUE)[1])


# converts categ_mapping in long format
categ_mapping_long <- gather(categ_mapping,"sector","Applicable",2:10)


# delete unneccessary rows
categ_mapping_long<- categ_mapping_long[!(categ_mapping_long$Applicable == 0),]


# dataframe with selected columns
sel_category_df <- select(categ_mapping_long,category_list,sector)


# merge category in master frame
merge_category <- merge(category_masterframe,sel_category_df,by.x="category_list",all.x=TRUE)


#==================Checkpoint 5 - END =====================



#==================Checkpoint 6 - START ===================

# Create 3 data frmaes with filters:
# country_code, funding_round_type, funding_amount_type

D1_frame <- merge_category %>% filter(country_code == top3$country_code[1]) %>% filter(funding_round_type == max_fundAmount_byTpe$funding_round_type) %>% filter(raised_amount_usd > 5000000 & raised_amount_usd < 15000000)
D2_frame <- merge_category %>% filter(country_code == top3$country_code[2]) %>% filter(funding_round_type == max_fundAmount_byTpe$funding_round_type) %>% filter(raised_amount_usd > 5000000 & raised_amount_usd < 15000000)
D3_frame <- merge_category %>% filter(country_code == top3$country_code[3]) %>% filter(funding_round_type == max_fundAmount_byTpe$funding_round_type) %>% filter(raised_amount_usd > 5000000 & raised_amount_usd < 15000000)


# aggregate first dataframe by main sector and number of investments
D1_frame_count <- D1_frame %>% group_by(sector) %>% summarise(cnt = length(!is.na(raised_amount_usd)))

# find sector for top 3 number of investments
D1_sector_top_investments <- D1_frame_count %>% top_n(3,cnt) %>% arrange(cnt)

# aggregate first dataframe by main sector and sum of investments
D1_frame_sum <- D1_frame %>% group_by(sector) %>% summarise(total = sum(raised_amount_usd)) %>% arrange(total)

# merge to add column for number of investments for each main sector
D1_count <- merge(D1_frame,D1_frame_count,by="sector")

# final merge to add column for total amount invested for each main sector 
D1 <- merge(D1_count,D1_frame_sum,by="sector")

# total investments for D1
D1_nrows <- nrow(D1)

# total amount invested for D1
D1_total_investment <- sum(D1$raised_amount_usd)

# find sector for top 3 number of investments
D1_sector_top_investments <- D1_frame_count %>% top_n(3,cnt) %>% arrange(cnt)

# filter D1 for top sector companies
D1_top1_sec_companies_frame <- D1 %>% filter(sector == D1_sector_top_investments$sector[3])

# find details for company with highest raised amount for sector with max investments
D1_top1_sec_company_row <- D1_top1_sec_companies_frame[which.max(D1_top1_sec_companies_frame$raised_amount_usd),]

#find name of company with highest raised amount for sector with max investments
D1_top1_sec_company <- D1_top1_sec_company_row$name

# filter D1 for second top sector companies
D1_top2_sec_companies_frame <- D1 %>% filter(sector == D1_sector_top_investments$sector[2])

# find details for company with highest raised amount for sector with second max investments
D1_top2_sec_company_row <- D1_top2_sec_companies_frame[which.max(D1_top2_sec_companies_frame$raised_amount_usd),]

#find name of company with highest raised amount for sector with second max investments
D1_top2_sec_company <- D1_top2_sec_company_row$name



# aggregate second dataframe by main sector and number of investments
D2_frame_count <- D2_frame %>% group_by(sector) %>% summarise(cnt = length(!is.na(raised_amount_usd)))

# find sector for top 3 number of investments
D2_sector_top_investments <- D2_frame_count %>% top_n(3,cnt) %>% arrange(cnt)

# aggregate first dataframe by main sector and sum of investments
D2_frame_sum <- D2_frame %>% group_by(sector) %>% summarise(total = sum(raised_amount_usd)) %>% arrange(total)

# merge to add column for number of investments for each main sector
D2_count <- merge(D2_frame,D2_frame_count,by="sector")

# final merge to add column for total amount invested for each main sector 
D2 <- merge(D2_count,D2_frame_sum,by="sector")

# total investments for D1
D2_nrows <- nrow(D2)

# total amount invested for D1
D2_total_investment <- sum(D2$raised_amount_usd)


# find sector for top 3 number of investments
D2_sector_top_investments <- D2_frame_count %>% top_n(3,cnt) %>% arrange(cnt)

# filter D3 for top sector companies
D2_top1_sec_companies_frame <- D2 %>% filter(sector == D2_sector_top_investments$sector[3])

# find details for company with highest raised amount for sector with max investments
D2_top1_sec_company_row <- D2_top1_sec_companies_frame[which.max(D2_top1_sec_companies_frame$raised_amount_usd),]

#find name of company with highest raised amount for sector with max investments
D2_top1_sec_company <- D2_top1_sec_company_row$name

# filter D2 for second top sector companies
D2_top2_sec_companies_frame <- D2 %>% filter(sector == D2_sector_top_investments$sector[2])

# find details for company with highest raised amount for sector with second max investments
D2_top2_sec_company_row <- D2_top2_sec_companies_frame[which.max(D2_top2_sec_companies_frame$raised_amount_usd),]

#find name of company with highest raised amount for sector with second max investments
D2_top2_sec_company <- D2_top2_sec_company_row$name




# aggregate third dataframe by main sector and number of investments
D3_frame_count <- D3_frame %>% group_by(sector) %>% summarise(cnt = length(!is.na(raised_amount_usd)))

# aggregate third dataframe by main sector and sum of investments
D3_frame_sum <- D3_frame %>% group_by(sector) %>% summarise(total = sum(raised_amount_usd)) %>% arrange(total)

# merge to add column for number of investments for each main sector
D3_count <- merge(D3_frame,D3_frame_count,by="sector")

# final merge to add column for total amount invested for each main sector 
D3 <- merge(D3_count,D3_frame_sum,by="sector")

# total investments for D1
D3_nrows <- nrow(D3)

# total amount invested for D1
D3_total_investment <- sum(D3$raised_amount_usd)

# find sector for top 3 number of investments
D3_sector_top_investments <- D3_frame_count %>% top_n(3,cnt) %>% arrange(cnt)

# filter D3 for top sector companies
D3_top1_sec_companies_frame <- D3 %>% filter(sector == D3_sector_top_investments$sector[3])

# find details for company with highest raised amount for sector with max investments
D3_top1_sec_company_row <- D3_top1_sec_companies_frame[which.max(D3_top1_sec_companies_frame$raised_amount_usd),]

#find name of company with highest raised amount for sector with max investments
D3_top1_sec_company <- D3_top1_sec_company_row$name

# filter D3 for second top sector companies
D3_top2_sec_companies_frame <- D3 %>% filter(sector == D3_sector_top_investments$sector[2])

# find details for company with highest raised amount for sector with second max investments
D3_top2_sec_company_row <- D3_top2_sec_companies_frame[which.max(D3_top2_sec_companies_frame$raised_amount_usd),]

#find name of company with highest raised amount for sector with second max investments
D3_top2_sec_company <- D3_top2_sec_company_row$name
