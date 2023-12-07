library(ggplot2)
library(dplyr)
library(corrplot)
# read data from loan.csv into datafrmae
loan_input <- read.csv("loan.csv")

######################### Data Cleaning ##############################


################# Delete Unnecessary columns##############

#Below columns are to be removed since they have all values as 'NA'or a single constan value:

# mths_since_last_major_derog, policy_code, application_type, annual_inc_joint, 
# dti_joint, verification_status_joint, acc_now_delinq, tot_coll_amt, tot_cur_bal, 
# open_acc_6m, open_il_6m, open_il_12m, open_il_24m, mths_since_rcnt_il, total_bal_il, il_util, 
# open_rv_12m, open_rv_24m, max_bal_bc, all_util, total_rev_hi_lim, inq_fi, total_cu_tl, 
# inq_last_12m, acc_open_past_24mths, avg_cur_bal, bc_open_to_buy, bc_util, delinq_amnt, 
# mo_sin_old_il_acct, mo_sin_old_rev_tl_op, mo_sin_rcnt_rev_tl_op, mo_sin_rcnt_tl, mort_acc, 
# mths_since_recent_bc, mths_since_recent_bc_dlq, mths_since_recent_inq, 
# mths_since_recent_revol_delinq, num_accts_ever_120_pd, num_actv_bc_tl, num_actv_rev_tl, 
# num_bc_sats, num_bc_tl, num_il_tl, num_op_rev_tl, num_rev_accts, num_rev_tl_bal_gt_0, 
# num_sats, num_tl_120dpd_2m, num_tl_30dpd, num_tl_90g_dpd_24m, num_tl_op_past_12m, 
# pct_tl_nvr_dlq, percent_bc_gt_75, tot_hi_cred_lim, total_bal_ex_mort, total_bc_limit, 
# total_il_high_credit_limit

# Note: chargeoff_within_12_mths and tax_liens columns are not removed since they have values '0' or 'NA'


loan_filteredColumns <- loan_input %>% select(-c(pymnt_plan,mths_since_last_major_derog, policy_code, 
                           application_type, annual_inc_joint, dti_joint, 
                           verification_status_joint, acc_now_delinq, 
                           tot_coll_amt, tot_cur_bal, open_acc_6m, 
                           open_il_6m, open_il_12m, open_il_24m, 
                           mths_since_rcnt_il, total_bal_il, il_util, open_rv_12m, 
                           open_rv_24m, max_bal_bc, all_util, total_rev_hi_lim, inq_fi, 
                           total_cu_tl, inq_last_12m, acc_open_past_24mths, avg_cur_bal, 
                           bc_open_to_buy, bc_util, delinq_amnt, mo_sin_old_il_acct, 
                           mo_sin_old_rev_tl_op, mo_sin_rcnt_rev_tl_op, mo_sin_rcnt_tl, 
                           mort_acc, mths_since_recent_bc, mths_since_recent_bc_dlq, 
                           mths_since_recent_inq, mths_since_recent_revol_delinq, 
                           num_accts_ever_120_pd, num_actv_bc_tl, num_actv_rev_tl, num_bc_sats, 
                           num_bc_tl, num_il_tl, num_op_rev_tl, num_rev_accts, num_rev_tl_bal_gt_0, 
                           num_sats, num_tl_120dpd_2m, num_tl_30dpd, num_tl_90g_dpd_24m, num_tl_op_past_12m, 
                           pct_tl_nvr_dlq, percent_bc_gt_75, tot_hi_cred_lim, total_bal_ex_mort, 
                           total_bc_limit, total_il_high_credit_limit))


# Columns 'int_rate' and 'revol_util' values are stripped of "%" sign
loan_filteredColumns$int_rate <- as.numeric(gsub("%","",as.character(loan_filteredColumns$int_rate)))
loan_filteredColumns$revol_util <- as.numeric(gsub("%","",as.character(loan_filteredColumns$revol_util)))

# Column 'term' values are to be stripped of "months" word
loan_filteredColumns$term <- as.numeric(gsub("months","",loan_filteredColumns$term))


# Filter values for loan_status = 'Charged Off'
loan_chargedOff <- loan_filteredColumns[which(loan_filteredColumns$loan_status == "Charged Off"),]

# Filter values for loan_status = 'Fully Paid'
loan_FullyPaid <- loan_filteredColumns[which(loan_filteredColumns$loan_status == 'Fully Paid'),]

int_rate_plot <- ggplot(loan_filteredColumns,aes(loan_filteredColumns$loan_status,fill=loan_filteredColumns$loan_status))+geom_histogram(stat="count") + labs(x = "Loan Status", y = "No. of Borrowers", title = "Loan Status", fill = "Loan Status ")

# Box plot to understand spread of interest rate for loan status
# This shows: Interest Rate of those who defaulted were higher compared to those who 'Fully Paid'.
# Inference: Interest Rate will have a impact if the borrower will default. 
#            Higher interest rate will lead to more defaults.
int_rate_boxplot <- boxplot(as.numeric(loan_filteredColumns$int_rate)~loan_filteredColumns$loan_status,loan_filteredColumns)

# Box plot to understand spread of funded amount for loan status
# This shows that there is negligible diffrence in funded amount for 'Charged Off' and 'Fully Paid'
# Inference: Funded Amount alone can't decide if the borrower will default.
fund_amt_boxplot <- boxplot(as.numeric(loan_filteredColumns$funded_amnt)~loan_filteredColumns$loan_status,loan_filteredColumns)

# Loan attribute
# Scatter Plor of funded amount vs interest rate.
# for 'Charged off' and 'Full Paid', interest rate were increased more rapidly compared to Current loan status.
loan_fundamt_scatrplot <- ggplot(loan_filteredColumns,aes(loan_filteredColumns$funded_amnt,loan_filteredColumns$int_rate, color=loan_filteredColumns$loan_status, shape = loan_filteredColumns$loan_status)) + geom_point(alpha=0.4) + geom_smooth() + labs(x = "Funded Amount", y = "Interest Rate", title = "Funded Amount Vs. Interest Rate", color = "Loan Status")

#loan_status_scatrplot <- ggplot(loan_filteredColumns,aes(log(loan_filteredColumns$id),loan_filteredColumns$int_rate, color=loan_filteredColumns$loan_status, shape = loan_filteredColumns$loan_status)) + geom_point() + geom_smooth() + labs(x = "Id log", y = "Interest Rate", title = "Loan Status Distribution", color = "Loan Status")

# positive correlation between interest rate and term.
# Loan attribute: as term increases so does the interest rate
#correlation_intRate_term <- cor(loan_filteredColumns$int_rate,as.numeric(loan_filteredColumns$term))
# 0.4516994

# For overall population, the bar plot shows that as grade moves from A to G,
# no. of borrowers decrease from A to G for 36 months terms,  
# for 60 months term grade B has higher number of borrowers.
# annual income remain same for all grades, but there are outliners
# Interest rate increases from grade A to grade G
# No of Borrowers have mainly property on RENT followed by Mortgaged
status_term_barplot <- ggplot(loan_filteredColumns,aes(loan_filteredColumns$grade,fill=factor(loan_filteredColumns$term)))+geom_bar(position = "dodge")
status_term_labeledbarplot <- status_term_barplot + labs(x = "Grade", y = "No. of Borrowers", title = "Overall Grade Term Distribution", fill="Term")
boxplot(log(loan_filteredColumns$annual_inc)~loan_filteredColumns$grade)
boxplot(loan_filteredColumns$int_rate~loan_filteredColumns$grade)
home_ownership_count_plot <- ggplot(loan_filteredColumns,aes(loan_filteredColumns$home_ownership,fill=factor(loan_filteredColumns$home_ownership)))+geom_bar()
home_ownership_labelledcount_plot <- home_ownership_count_plot + labs(x="Home Ownership",y="No. of Borrowers",title="Overall Home Ownership Distribution",fill="Home Ownership")

# Out of overall population, for 36 month term grade B group defaulted most and then decreased till grade G
# For 60 months term, grade D and grade E has highest number of defaulters.
# No of Borrowers who defaulted have mainly property on RENT followed by Mortgaged
charged_term_barplot <- ggplot(loan_chargedOff,aes(loan_chargedOff$grade,fill=factor(loan_chargedOff$term)))+geom_bar(position = "dodge")
charged_term_labeledbarplot <- charged_term_barplot + labs(x = "Grade", y = "No. of Borrowers", title = "Charged Off Grade Term Distribution", fill="Term")
home_ownership_count_chargedplot <- ggplot(loan_chargedOff,aes(loan_chargedOff$home_ownership,fill=factor(loan_chargedOff$home_ownership)))+geom_bar()
home_ownership_labelledcount_chargedplot <- home_ownership_count_chargedplot + labs(x="Home Ownership",y="No. of Borrowers",title="Home Ownership Distribution - Charged Off",fill="Home Ownership")


# create a dataframe with grade B for defaulted borrowers
charged_gradeB_dataframe <- loan_chargedOff[which(loan_chargedOff$grade == "B"),]

# interest rate for defaulters of grade B are almost equally spreaded for 36 and 60 months
# Funding Amount for grade B for 60 months is higher than that for 36 months.
# Number of defaulters for grade B is more for 36 months compared to 60 months
# No of Borrowers with grade B who defaulted have mainly property on RENT followed by Mortgaged
boxplot(charged_gradeB_dataframe$int_rate~charged_gradeB_dataframe$term,fill =factor(charged_gradeB_dataframe$term))
boxplot(charged_gradeB_dataframe$funded_amnt~charged_gradeB_dataframe$term,fill =factor(charged_gradeB_dataframe$term))
charged_gradeB_term_count <- ggplot(charged_gradeB_dataframe,aes(charged_gradeB_dataframe$term,fill=factor(charged_gradeB_dataframe$term)))+geom_histogram()
charged_gradeB_term_labelledcount <- charged_gradeB_term_count + labs(x="Term", y= "No. of Borrowers", title ="Term Grade B Distributions", fill="Term")
home_ownership_count_charged_grBplot <- ggplot(charged_gradeB_dataframe,aes(charged_gradeB_dataframe$home_ownership,fill=factor(charged_gradeB_dataframe$home_ownership)))+geom_bar()
home_ownership_labelledcount_charged_grBplot <- home_ownership_count_charged_grBplot + labs(x="Home Ownership",y="No. of Borrowers",title="Home Ownership Distribution - Charged Off Grade B",fill="Home Ownership")


# create a dataframe with grade B and term of 36 months for defaulted borrowers
charged_gradeB_term3yr_dataframe <- loan_chargedOff[which(loan_chargedOff$term == 36 & loan_chargedOff$grade == "B"),]

# for defaulter population, annual income remain same for all grades, but there are outliners
# Interest rate increases from grade A to grade G
# Funding amount increases from grade A to grade G
boxplot(log(loan_chargedOff$annual_inc)~loan_chargedOff$grade,fill =factor(loan_chargedOff$grade))
boxplot(loan_chargedOff$int_rate~loan_chargedOff$grade,fill =factor(loan_chargedOff$grade))
boxplot(loan_chargedOff$funded_amnt~loan_chargedOff$grade,fill =factor(loan_chargedOff$grade))

# No of Borrowers with grade B , term of 36 months and who defaulted have mainly property on RENT followed by Mortgaged
home_ownership_count_charged_grB_3yr_plot <- ggplot(charged_gradeB_term3yr_dataframe,aes(charged_gradeB_term3yr_dataframe$home_ownership,fill=factor(charged_gradeB_term3yr_dataframe$home_ownership)))+geom_bar()
home_ownership_labelledcount_charged_grB_3yr_plot <- home_ownership_count_charged_grB_3yr_plot + labs(x="Home Ownership",y="No. of Borrowers",title="Home Ownership Distribution - Charged Off Grade B Term 3 yr",fill="Home Ownership")

# create a dataframe with defaulted borrowers for grade B, term of 36 months and property on RENT
charged_grade_term_prop_dframe <- charged_gradeB_term3yr_dataframe[which(charged_gradeB_term3yr_dataframe$home_ownership =="RENT"),]

# Most Defaulters of grade B,term 36 months and property on RENT have 'Non Verified' status
charged_grade_term_prop_verstatus_plot <- ggplot(charged_grade_term_prop_dframe,aes(charged_grade_term_prop_dframe$verification_status,fill=factor(charged_grade_term_prop_dframe$verification_status)))+geom_bar()
labelled_verStatus_plot <- charged_grade_term_prop_verstatus_plot +labs(x="Verification Status", y="No. of Borrowers",title ="Defaulter Vs Verification Staus", fill = "Verification status")

# create a dataframe with grade B,term 36 months ,property on RENT ,'Non Verified' status 
filterd_verStatus <- charged_grade_term_prop_dframe[which(charged_grade_term_prop_dframe$verification_status == "Not Verified"),]

# Defaulters with grade B,term 36 months ,property on RENT ,'Non Verified' status are more from CA, followed by FL and NY
state_plot <- ggplot(filterd_verStatus,aes(filterd_verStatus$addr_state,fill=factor(filterd_verStatus$addr_state)))+geom_bar()
labelled_stateplot <- state_plot + labs(x="States", y = "No. of Borrowers", title="States Distribution", fill="State")

# create a dataframe with grade B,term 36 months ,property on RENT ,'Non Verified' status, state CA
filtered_state <- filterd_verStatus[which(filterd_verStatus$addr_state == "CA"),]

# Defaulters from CA with grade B,term 36 months ,property on RENT ,'Non Verified' status 
# have loan purpose 'debt consolidation' 
purpose_plot <- ggplot(filtered_state,aes(filtered_state$purpose,fill=factor(filtered_state$purpose)))+geom_bar()
labelled_purposeplot <- purpose_plot + labs(x="Purpose", y = "No. of Borrowers", title="Purpose Distribution", fill="Purpose")

filtered_purpose <- filtered_state[which(filtered_state$purpose == "debt_consolidation"),]


# Findings from Correlation Matrix:
# 1: Positive correlation between open_acc and dti
# 2. Positive correlation between total_acc and open_acc
# 3. Positive Corelation between loan amount and annual income 
# 4. Negative corelation between public records and loan amt
# 5. delinquency for 2 years margin increase with di

cor_matrix_dataframe <- filtered_purpose %>% select(c(3:8),annual_inc,dti,delinq_2yrs,c(30:34),c(37:44),last_pymnt_amnt)
cor_matrix <- round(cor(cor_matrix_dataframe),2)
png(height=1200, width=1200, pointsize=25, file="correlation.png")
color <- colorRampPalette(c("#7F0000","red","#FF7F00","yellow","#7FFF7F", 
                            "cyan", "#007FFF", "blue","#00007F")) 
#cor_plot <- corrplot(cor_matrix, na.label="NA",method="number",number.cex=1,addCoef.col = "red", title = "Correalation Matrix")
cor_plot <- corrplot(cor_matrix, na.label="NA",method="number",col=color(10),number.cex = .70, title = "Correalation Matrix")
dev.off()



cor_matrix_dataframe1 <- loan_filteredColumns %>% select(c(3:8),annual_inc,dti,delinq_2yrs,c(30:34),c(37:44),last_pymnt_amnt)
cor_matrix1 <- round(cor(cor_matrix_dataframe1),2)
png(height=1200, width=1200, pointsize=25, file="correlation1.png")
color <- colorRampPalette(c("#7F0000","red","#FF7F00","yellow","#7FFF7F", 
                           "cyan", "#007FFF", "blue","#00007F")) 
cor_plot1 <- corrplot(cor_matrix1, na.label="NA",method="number",col=color(10),number.cex = .70, title = "Correalation Matrix")
dev.off()


# Defaulters from CA with grade B,term 36 months ,property on RENT ,'Non Verified' status and
# loan_amount less than 15000 has total payment increasing, excluding outliner
# For overall population, total payment always increase with loan_amount
loanamt_totpymt_plot <- ggplot(filtered_purpose,aes(filtered_purpose$loan_amnt,filtered_purpose$total_pymnt))+ geom_point()+ geom_smooth()+labs(x="Loan Amount",y="Total Payment",title="Total Pymnt Vs Loan Amount")

# For Defaulters from CA with grade B,term 36 months ,property on RENT ,'Non Verified' status 
# increase in loan amount is not changing dti
# Similar for overall population
loanamt_dti_plot <- ggplot(filtered_purpose,aes(filtered_purpose$loan_amnt,filtered_purpose$dti))+ geom_point()+ geom_smooth()+labs(x="Loan Amt",y="dti",title="Loan Amt Vs DTI")


# For Defaulters from CA with grade B,term 36 months ,property on RENT ,'Non Verified' status 
# higher annual income has higher loan amount
# For overall population same trend observed when viewing using Power Law
loanamt_aninc_plot <- ggplot(filtered_purpose,aes(filtered_purpose$loan_amnt,filtered_purpose$annual_inc))+ geom_point()+ geom_smooth()+labs(x="Loan Amt",y="Annual Inc",title="Loan Amt Vs Annual Inc")

# For Defaulters from CA with grade B,term 36 months ,property on RENT ,'Non Verified' status 
# open credit lines increase with annual income
# For overall population, excluding outliners with annual income greater than 60000,
# trend remains the same.
openacc_aninc_plot <- ggplot(filtered_purpose,aes(filtered_purpose$annual_inc,filtered_purpose$open_acc))+ geom_point()+ geom_smooth()+labs(x="Annual Inc",y="Open Acc",title="Annual Inc Vs Open Acc")
outline_aninc_limit <- median(loan_filteredColumns$annual_inc)
outline_anincdf <- loan_filteredColumns[which(loan_filteredColumns$annual_inc < outline_aninc_limit),]
openacc_aninc_overallplot <- ggplot(outline_anincdf,aes(outline_anincdf$annual_inc,outline_anincdf$open_acc))+ geom_point()+ geom_smooth()+labs(x="Annual Inc",y="Open Acc",title="Overall Annual Inc Vs Open Acc")

# For Defaulters from CA with grade B,term 36 months ,property on RENT ,'Non Verified' status 
# dti ratio increase with open credit lines
# For overall population, removing outliners, trend remains the same
openacc_dti_plot <- ggplot(filtered_purpose,aes(filtered_purpose$open_acc,filtered_purpose$dti))+ geom_point()+ geom_smooth()+labs(x="Open Acc",y="dti",title="Open Acc Vs DTI")

ggplot(loan_raw,aes(loan_raw$open_acc,loan_raw$int_rate))+ geom_point()+ geom_smooth()+labs(x="Open Acc",y="Interest rate",title="Open Acc Vs Interest rate")