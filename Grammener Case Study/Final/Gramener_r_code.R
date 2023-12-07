#----------------------------Group_Case_Study_2_Lending Club--------------------------------#
#Loading the required libraries#
library(ggplot2)
library(dplyr)
library(tidyr)
library(lubridate)
library(data.table)
library(stringr)
library(corrplot)
#-------------------------------------------------------------------------------------------#
setwd("D:/Upgrad/Case_Study_3_Lending Club")
#Loading loan file to R console#
loan_raw <- read.csv("loan.csv",header = TRUE, sep = ",", stringsAsFactors = FALSE)
str(loan_raw)
#dropping or removing the columns that have only NA as the value, these are not required#
loan_raw <- loan_raw[,-c(51,54:78,80:105,107:111)]
#----------------------Column F (term)-----------------------------#
#trimming the leading white spaces in term column#
loan_raw$term <- str_trim(loan_raw$term)
#Mutating the column term to remove months from each row, so as to make it a calculation field#
loan_raw <- mutate(loan_raw,term = sapply(strsplit(loan_raw$term, split=' ', fixed=TRUE),function(x) (x[1])))
#Converting column term to numeric#
loan_raw$term <- as.numeric(loan_raw$term)
#----------------------Column L (emp_length)-----------------------------#
#replacing < 1 year to "Below 1 Year" for better readability#
loan_raw$emp_length <- str_replace_all(loan_raw$emp_length,c("< 1 year" = "Below 1 Year"))
#-------------Column G & AH (int_rate & revol_util)----------------------#
# Columns 'int_rate' and 'revol_util' values are stripped of "%" sign
loan_raw$int_rate <- as.numeric(gsub("%","",as.character(loan_raw$int_rate)))
loan_raw$revol_util <- as.numeric(gsub("%","",as.character(loan_raw$revol_util)))
#-------------------Univariate & Bivariate Analysis----------------------#
# Univariate analysis on loan status variable.
# This shows that data given mainly comprises of 'Fully Paid" loans 
# followed by "Charged Off" and then "Current" loans.
loan_status_plot <- ggplot(loan_raw, aes(x = loan_raw$loan_status, fill=loan_raw$loan_status))
label_int_rate <- labs(x = "Loan Status", y = "Number of Borrowers", title = "Loan Status", fill = "Loan Status ")
loan_status_plot_final <- loan_status_plot + label_int_rate + geom_histogram(stat="count")
#------------------------------------------------------------------------#
# Box plot to understand spread of interest rate for loan status
# This shows: Interest Rate of those who defaulted were higher compared to those who 'Fully Paid'.
# Inference: Interest Rate will have a impact if the borrower will default. 
#            Higher interest rate will lead to more defaults.
int_rate_boxplot <- boxplot(as.numeric(loan_raw$int_rate)~loan_raw$loan_status,loan_raw)
#------------------------------------------------------------------------#
# Box plot to understand spread of funded amount for loan status
# This shows that there is negligible difference in funded amount for 'Charged Off' and 'Fully Paid'
# Inference: Funded Amount alone can't decide if the borrower will default.
funded_amt_boxplot <- boxplot(as.numeric(loan_raw$funded_amnt)~loan_raw$loan_status,loan_raw)
#------------------------------------------------------------------------#
# Loan attribute
# Scatter Plor of funded amount vs interest rate.
# for 'Charged off' and 'Full Paid', interest rate were increased more rapidly compared to Current loan status.
loan_fundedamt_scatterplot <- ggplot(loan_raw,aes(loan_raw$funded_amnt,loan_raw$int_rate, color=loan_raw$loan_status, shape = loan_raw$loan_status))
label_funded_amount <- labs(x = "Funded Amount", y = "Interest Rate", title = "Funded Amount Vs. Interest Rate", color = "Loan Status")
loan_fundedamt_scatterplot_final <- loan_fundedamt_scatterplot + label_funded_amount + geom_point(alpha=0.4) + geom_smooth()
#------------------------------------------------------------------------#
# For overall population, the bar plot shows that as grade moves from A to G,
# number of borrowers decrease from A to G for 36 months terms,  
# for 60 months term grade B has higher number of borrowers.
status_term_barplot <- ggplot(loan_raw,aes(loan_raw$grade,fill=factor(loan_raw$term)))
labels_status_term <- labs(x = "Grade", y = "No. of Borrowers", title = "Overall Grade Term Distribution", fill="Term")
status_term_labeled_barplot_final <- status_term_barplot + labels_status_term + geom_bar(position = "dodge")
#------------------------------------------------------------------------#
# the below box plot shows that the annual income remain same for all grades, but there are outliers
boxplot(log(loan_raw$annual_inc)~loan_raw$grade)
#------------------------------------------------------------------------#
# the below box plot shows that interest rate increases from grade A to grade G
boxplot(loan_raw$int_rate~loan_raw$grade)
#------------------------------------------------------------------------#
# the below plot shows that the number of borrowers have mainly property on rent followed by mortgaged
home_ownership_count_plot <- ggplot(loan_raw,aes(loan_raw$home_ownership,fill=factor(loan_raw$home_ownership)))
label_home_ownership <- labs(x="Home Ownership",y="Number of Borrowers",title="Home Ownership Distribution",fill="Home Ownership")
home_ownership_plot_final <- home_ownership_count_plot + label_home_ownership +geom_bar()

#--------------------------Segmented Analysis----------------------------------------#

# Subsetting on Charged off to find if there is any possible default pattern
loan_chargedOff <- loan_raw[which(loan_raw$loan_status == "Charged Off"),]
#------------------------------------------------------------------------------------#
# Out of overall population, for 36 month term grade B group defaulted most and then decreased till grade G
# For 60 months term, grade D and grade E has highest number of defaulters.
charged_term_barplot <- ggplot(loan_chargedOff,aes(loan_chargedOff$grade,fill=factor(loan_chargedOff$term)))
label_charged_term <- labs(x = "Grade", y = "Number of Borrowers", title = "Charged Off, Grade & Term Distribution", fill="Term in months")
charged_term_barplot_final <- charged_term_barplot + label_charged_term + geom_bar(position = position_dodge(width = 0.5))
#------------------------------------------------------------------------------------#
# No of Borrowers who defaulted have mainly property on RENT followed by Mortgaged
home_ownership_count_chargedplot <- ggplot(loan_chargedOff,aes(loan_chargedOff$home_ownership,fill=factor(loan_chargedOff$home_ownership)))+geom_bar()
label_charged_home_ownership <- labs(x="Home Ownership",y="No. of Borrowers",title="Home Ownership Distribution - Charged Off",fill="Home Ownership")
home_ownership_labelledcount_chargedplot <- home_ownership_count_chargedplot + label_charged_home_ownership
#------------------------------------------------------------------------------------#
# create a dataframe with grade B and term of 36 months for defaulted borrowers
charged_gradeB_term3yr_dataframe <- loan_chargedOff[which(loan_chargedOff$term == 36 & loan_chargedOff$grade == "B"),]
#------------------------------------------------------------------------------------#
# for defaulter population, annual income remain same for all grades, but there are outliers
boxplot(log(loan_chargedOff$annual_inc)~loan_chargedOff$grade,fill =factor(loan_chargedOff$grade))
#------------------------------------------------------------------------------------#
# Interest rate increases from grade A to grade G
boxplot(loan_chargedOff$int_rate~loan_chargedOff$grade,fill =factor(loan_chargedOff$grade))
#------------------------------------------------------------------------------------#
# Funding amount increases from grade A to grade G
boxplot(loan_chargedOff$funded_amnt~loan_chargedOff$grade,fill =factor(loan_chargedOff$grade))
# Above trends are same for overall population
#------------------------------------------------------------------------------------#
# No of Borrowers with grade B , term of 36 months and who defaulted 
# have mainly property on RENT followed by Mortgaged
home_ownership_count_charged_grB_3yr_plot <- ggplot(charged_gradeB_term3yr_dataframe,aes(charged_gradeB_term3yr_dataframe$home_ownership,fill=factor(charged_gradeB_term3yr_dataframe$home_ownership)))+geom_bar()
label_charged_home_ownership_3yrs <- labs(x="Home Ownership",y="No. of Borrowers",title="Home Ownership Distribution - Charged Off Grade B Term 3 yr",fill="Home Ownership")
home_ownership_labelledcount_charged_grB_3yr_plot <- home_ownership_count_charged_grB_3yr_plot + label_charged_home_ownership_3yrs
#------------------------------------------------------------------------------------#
# create a dataframe with defaulted borrowers for grade B, term of 36 months and property on RENT
charged_grade_term_prop_dframe <- charged_gradeB_term3yr_dataframe[which(charged_gradeB_term3yr_dataframe$home_ownership =="RENT"),]
#------------------------------------------------------------------------------------#
# Most Defaulters of grade B,term 36 months and property on RENT have 'Non Verified' status
# For overall population also, no. of Borrowers with 'Non Verified' are more
charged_grade_term_prop_verstatus_plot <- ggplot(charged_grade_term_prop_dframe,aes(charged_grade_term_prop_dframe$verification_status,fill=factor(charged_grade_term_prop_dframe$verification_status)))+geom_bar()
labelled_verStatus <- labs(x="Verification Status", y="No. of Borrowers",title ="Defaulter Vs Verification Staus", fill = "Verification status")
charged_grade_term_prop_verstatus_plot_final <- charged_grade_term_prop_verstatus_plot + labelled_verStatus
#------------------------------------------------------------------------------------#
# create a dataframe with grade B,term 36 months ,property on RENT ,'Non Verified' status 
filterd_verStatus <- charged_grade_term_prop_dframe[which(charged_grade_term_prop_dframe$verification_status == "Not Verified"),]
#------------------------------------------------------------------------------------#
# Defaulters with grade B,term 36 months ,property on RENT ,'Non Verified' status are more from CA, followed by FL and NY
state_plot <- ggplot(filterd_verStatus,aes(filterd_verStatus$addr_state,fill=factor(filterd_verStatus$addr_state)))+geom_bar()
labelled_stateplot <- state_plot + labs(x="States", y = "No. of Borrowers", title="States Distribution", fill="State")
#------------------------------------------------------------------------------------#
# create a dataframe with grade B,term 36 months ,property on RENT ,'Non Verified' status, state CA
filtered_state <- filterd_verStatus[which(filterd_verStatus$addr_state == "CA"),]
#------------------------------------------------------------------------------------#
# Defaulters from CA with grade B,term 36 months ,property on RENT ,'Non Verified' status 
# have loan purpose 'debt consolidation' 
purpose_plot <- ggplot(filtered_state,aes(filtered_state$purpose,fill=factor(filtered_state$purpose)))+geom_bar()
legend_position_1 <- theme(legend.position = "bottom")
labelled_purposeplot <- purpose_plot + labs(x="Purpose", y = "No. of Borrowers", title="Purpose Distribution", fill="Purpose") + legend_position_1
#------------------------------------------------------------------------------------#
#create a dataframe with grade B,term 36 months ,property on RENT ,'Non Verified' status, state CA & purpose as debt consolidation
filtered_purpose <- filtered_state[which(filtered_state$purpose == "debt_consolidation"),]
#------------------------------------------------------------------------------------#
# Defaulters from CA with grade B,term 36 months ,property on RENT ,'Non Verified' status and
# loan_amount less than 15000 has total payment increasing, excluding outliers
# For overall population, total payment always increase with loan_amount
loanamt_totpymt_plot <- ggplot(filtered_purpose,aes(filtered_purpose$loan_amnt,filtered_purpose$total_pymnt))+ geom_point()+ geom_smooth()
label_loanamt_totpymt <- labs(x="Loan Amount",y="Total Payment",title="Total Payment Vs Loan Amount")
loanamt_totpymt_plot_final <- loanamt_totpymt_plot + label_loanamt_totpymt
#------------------------------------------------------------------------------------#
# For Defaulters from CA with grade B,term 36 months ,property on RENT ,'Non Verified' status 
# increase in loan amount is not changing dti
# Similar for overall population
loanamt_dti_plot <- ggplot(filtered_purpose,aes(filtered_purpose$loan_amnt,filtered_purpose$dti))+ geom_point()+ geom_smooth()
label_loanamt_dti <- labs(x="Loan Amt",y="dti",title="Loan Amt Vs DTI")
loanamt_dti_plot_final <- loanamt_dti_plot + label_loanamt_dti
#-----------------------------------------------------------------------------------#
# Default borrowers with no public records are given loans most.
pub_rec_plot <-ggplot(loan_chargedOff,aes(loan_chargedOff$pub_rec,fill= factor(loan_chargedOff$pub_rec)))+ geom_bar()
label_pub_rec <- labs(x="Open Acc",y="No.of borrowers",title="Open Acc Vs Delinquency",fill="No. of Public Records")
pub_rec_plot_final <- pub_rec_plot + label_pub_rec

#---------------------------Multivariate Analysis------------------------------#

#Understanding the relationship between loan_status, Verification_Status, grades & funded_amt #
#Finding the summary of different grades, taking into consideration loan and verification status#
#Finding the count in each bucket#
summary_countwise <- tally(group_by(loan_raw, loan_status, verification_status, grade))
#Finding the amount attached in each bucket#
summary_amountwise <- as.data.frame(summarise(group_by(loan_raw, loan_status, verification_status, grade),sum(funded_amnt)))
#renaming the amount column for better understanding#
colnames(summary_amountwise)[colnames(summary_amountwise)=="sum(funded_amnt)"] <- "Amount"
#Converting amount to thousands for better readability in graph#
summary_amountwise$Amount <- summary_amountwise$Amount/1000

#Plotting summary of number of loans sanctioned in different grades#
#taking into consideration the verification type and loan status assigned to it#
#----------------------------PLOT FOR COUNTWISE-------------------------#
summary_countwise_plot <- ggplot(summary_countwise, aes(x = loan_status, y = n, col = verification_status, shape = grade))
#As there are more than 7 grades hence plotting it manually by creating a variable#
change_shape <- scale_shape_manual(values = c(24, 15, 16, 17, 3, 8, 4))
#adding a background color for better visibility#
background_fill <- theme(panel.background = element_rect(fill = 'black'))
#creating a variable called add labels to define labels#
add_labels_1 <- labs(title = "Summary of Verification & Loan Status on Grade (COUNTWISE)", x = "Loan Status", y = "Count Of Loans")
#moving the legends from the right side to bottom for better readability#
legend_position <- theme(legend.position = "bottom")
#Creating the final plot#
summary_countwise_plot_final <- summary_countwise_plot + geom_point(alpha = 0.7,position = position_jitter(width = 0.4)) + change_shape + add_labels_1 + legend_position + background_fill

#Plotting summary of amount in loans sanctioned in different grades#
#taking into consideration the verification type and loan status assigned to it#
#----------------------------PLOT FOR AMOUNTWISE-------------------------#
summary_amountwise_plot <- ggplot(summary_amountwise, aes(x = loan_status, y = Amount, col = verification_status, shape = grade))
#creating a variable called add labels to define labels#
add_labels_2 <- labs(title = "Summary of Verification & Loan Status on Grade (AMOUNTWISE)", x = "Loan Status", y = "Funded Amount in thousands")
#Creating the final plot#
summary_amountwise_plot_final <- summary_amountwise_plot + geom_point(alpha = 0.7,position = position_jitter(width = 0.4)) + change_shape + add_labels_2 + legend_position + background_fill

#-----------------------Termwise Analysis--------------------------------#
#Plotting summary of amount in loans sanctioned in different grades#
#taking into consideration the term and loan status assigned to it#
summary_termwise <- as.data.frame(summarise(group_by(loan_raw, loan_status, term, grade),sum(funded_amnt)))
#renaming the amount column for better understanding#
colnames(summary_termwise)[colnames(summary_termwise)=="sum(funded_amnt)"] <- "Amount"
#Converting amount to millions for better readability in graph#
summary_termwise$Amount <- summary_termwise$Amount/1000000


summary_termwise_plot <- ggplot(summary_termwise, aes(x = factor(term), y = Amount, col = loan_status, shape = grade))
#creating a variable called add labels to define labels#
add_labels_3 <- labs(title = "Summary of Term & Loan Status on Grade (AMOUNTWISE)", x = "Term", y = "Funded Amount in millions")
#Creating the final plot#
summary_termwise_plot_final <- summary_termwise_plot + geom_point(alpha = 0.7,position = position_jitter(width = 0.4)) + change_shape + add_labels_3 + legend_position + background_fill

#-------------------Employee Lengthwise Analysis---------------------------#
#Plotting summary of amount in loans sanctioned in different grades#
#taking into consideration the employement length and loan status assigned to it#
summary_emplengthwise <- as.data.frame(summarise(group_by(loan_raw, loan_status, emp_length, grade),sum(funded_amnt)))
#renaming the amount column for better understanding#
colnames(summary_emplengthwise)[colnames(summary_emplengthwise)=="sum(funded_amnt)"] <- "Amount"
#Converting amount to thousands for better readability in graph#
summary_emplengthwise$Amount <- summary_emplengthwise$Amount/1000


summary_emplengthwise_plot <- ggplot(summary_emplengthwise, aes(x = factor(emp_length), y = Amount, col = loan_status, shape = grade))
#creating a variable called add labels to define labels#
add_labels_4 <- labs(title = "Summary of Employee Tenure & Loan Status on Grade (AMOUNTWISE)", x = "Employee Tenure", y = "Funded Amount in thousands")
#Creating the final plot#
summary_emplengthwise_plot_final <- summary_emplengthwise_plot + geom_point(alpha = 0.7,position = position_jitter(width = 0.35)) + change_shape + add_labels_4 + legend_position + background_fill

#-------------------House ownershipwise Analysis---------------------------#
#Plotting summary of amount in loans sanctioned in different grades#
#taking into consideration the home ownership and loan status assigned to it#
summary_houseownershipwise <- as.data.frame(summarise(group_by(loan_raw, loan_status, home_ownership, grade),sum(funded_amnt)))
#renaming the amount column for better understanding#
colnames(summary_houseownershipwise)[colnames(summary_houseownershipwise)=="sum(funded_amnt)"] <- "Amount"
#Converting amount to thousands for better readability in graph#
summary_houseownershipwise$Amount <- summary_houseownershipwise$Amount/1000


summary_houseownershipwise_plot <- ggplot(summary_houseownershipwise, aes(x = factor(home_ownership), y = Amount, col = loan_status, shape = grade))
#creating a variable called add labels to define labels#
add_labels_5 <- labs(title = "Summary of House Ownership & Loan Status on Grade (AMOUNTWISE)", x = "House Ownership", y = "Funded Amount in thousands")
#Creating the final plot#
summary_houseownershipwise_plot_final <- summary_houseownershipwise_plot + geom_point(alpha = 0.7,position = position_jitter(width = 0.4)) + change_shape + add_labels_5 + legend_position + background_fill

#------------------------Purposewise Analysis-----------------------------#
#Plotting summary of amount in loans sanctioned in different grades#
#taking into consideration the purpose and loan status assigned to it#
summary_purposewise <- as.data.frame(summarise(group_by(loan_raw, loan_status, purpose, grade),sum(funded_amnt)))
#renaming the amount column for better understanding#
colnames(summary_purposewise)[colnames(summary_purposewise)=="sum(funded_amnt)"] <- "Amount"
#Converting amount to millions for better readability in graph#
summary_purposewise$Amount <- summary_purposewise$Amount/1000000


summary_purposewise_plot <- ggplot(summary_purposewise, aes(x = Amount, y = factor(purpose), col = loan_status, shape = grade))
#creating a variable called add labels to define labels#
add_labels_6 <- labs(title = "Summary of Purpose & Loan Status on Grade (AMOUNTWISE)", x = "Funded Amount in millions", y = "Purpose")
#Creating the final plot#
summary_purposewise_plot_final <- summary_purposewise_plot + geom_point(alpha = 0.7,position = position_jitter(width = 0.35)) + change_shape + add_labels_6 + legend_position + background_fill

#---------------------------Correlation Matrix---------------------------#

# Findings from Correlation Matrix:
# 1: Strong Positive Corelation between loan amount and total payment.
# 2. Strong Positive Corelation between loan amount and funded amount.
# 3. Strong Positive Corelation between loan amount and total payment invested
# 4. Negative corelation between public records and loan amt
# 5. No relation between DTI ratio and loan amount
# 6. Negative corelation between pub_record and delinquency after 2 years 
cor_matrix_dataframe1 <- loan_raw %>% select(c(3:8),annual_inc,dti,delinq_2yrs,c(30:34),c(37:44),last_pymnt_amnt)
cor_matrix1 <- round(cor(cor_matrix_dataframe1),2)
png(height=1200, width=1200, pointsize=25, file="correlation.png")
color <- colorRampPalette(c("#7F0000","red","#FF7F00","yellow","#7FFF7F", "cyan", "#007FFF", "blue","#00007F")) 
cor_plot1 <- corrplot(cor_matrix1, na.label="NA",method="number",col=color(10),number.cex = .70, title = "Correalation Matrix")
dev.off()

#---------------------------Quantative Variables-------------------------#
#Impact of open credit lines along with grades#
#Plotting open accounts to understand its effect#
Summary_openacc <- ggplot(loan_raw, aes(x= open_acc, fill = factor(grade))) + geom_bar(alpha = 0.6)
add_labels_7 <- labs(title = "Summary of Open accounts & Grade (COUNTWISE)", x = "Open credit Lines", y = "Count of Open Accounts")
Summary_openacc_plot <- Summary_openacc + add_labels_7 + scale_fill_discrete(name = "Grade")
#We see that minimum credit line for eligibility is 2#
#As the number of credit line increases the number of loan decreases#
#Usually people having 8 to 10 open credit lines get loans easily#
Summary_intrate_openacc <- ggplot(loan_raw, aes(x = int_rate, col = factor(open_acc))) + geom_freqpoly(binwidth = 1, position = "identity", size = 0.15)
add_labels_8 <- labs(title = "Frequency of Open accounts & Interest rate (COUNTWISE)", x = "Interest rate", y = "Count of loans in each open accounts")
Summary_intrate_openacc_plot <- Summary_intrate_openacc + add_labels_8 + scale_color_discrete(name = "Open Credit Lines")
#the above frequency chart says that the interest rate charged#
#across all the open credit lines seems to follow the same pattern#
#hence number of open credit lines do not have any impact on interest rates#
summary_fundedamnt_open_acc_chargeoff <- ggplot(loan_chargedOff, aes(x = funded_amnt, col = factor(open_acc)))
add_labels_9 <- labs(title = "Share of open accounts in funded amount for charged off", x = "Funded Amount", y = "Count of loans in each open accounts")
summary_fundedamnt_open_acc_chargeoff_final <- summary_fundedamnt_open_acc_chargeoff + add_labels_9  + geom_histogram() + scale_color_discrete(name = "Open Credit Lines")
#looking at amount charged off in different open credit lines#
#we see that the stackes for open credit lnes that fall between 2 to 6#
#have comparatively higher default amount attached to them#
#Hence we can say that customers with 2 to 6 open credit lines are risky one's#
#-----------------------------------------------------------------------#
Summary_delinquent <- ggplot(loan_raw, aes(x= grade, fill = factor(delinq_2yrs))) + geom_bar(alpha = 0.6)
add_labels_10 <- labs(title = "Customer delinquencies distribution", x = "Grade", y = "Count of loans")
Summary_delinquent_plot <- Summary_delinquent + add_labels_10 + scale_fill_discrete(name = "No of times customer failed to pay in time") + legend_position
#looking at the above graph we see overall very few customers have failed to pay in time#
Summary_intrate_deliquent <- ggplot(loan_raw, aes(x = int_rate, col = factor(delinq_2yrs))) + geom_freqpoly(binwidth = 1, position = "identity", size = 0.15)
add_labels_11 <- labs(title = "Frequency of Delinquent & Int rate (COUNTWISE)", x = "Interest rate", y = "Count of loans")
Summary_intrate_deliquent_plot <- Summary_intrate_deliquent + add_labels_11 + scale_color_discrete(name = "No of times customer failed to pay in time") + legend_position
#the above frequency chart says that the interest rate charged#
#across all the delinquencies seems to follow the same pattern#
#hence number of delinquencies do not have any impact on interest rates#
summary_fundedamnt_delinquent_chargeoff <- ggplot(loan_chargedOff, aes(x = funded_amnt, col = factor(delinq_2yrs)))
add_labels_12 <- labs(title = "Share of delinquent in funded amount for charged off", x = "Funded Amount", y = "Count of loans")
summary_fundedamnt_delinquent_chargeoff_final <- summary_fundedamnt_delinquent_chargeoff + add_labels_12  + geom_histogram() + scale_color_discrete(name = "No of times customer failed to pay in time") + legend_position
#looking at amount charged off in different delinquencies#
#we see that the stackes for delinquencies that fall in 1 to 2#
#have comparatively higher default amount attached to them#
#Hence we can say that customers with 1 to 2 delinquencies are risky one's#
#-------------------------------End of Code------------------------------------------#