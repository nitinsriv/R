cens <- read.csv("C:\\IIITB\\EDA\\EDA_census_changed.csv")
ilit_2024_Ind<-cens[which(cens$Age.group == "20-24" & cens$Area.Name == "INDIA" & cens$Total.Rural.Urban == "Total"),]
per <- ilit_2024_Ind$IF/ilit_2024_Ind$TF

cens_ind <- cens[which(cens$Area.Name == "INDIA" & cens$Total.Rural.Urban == "Total"),]
#cens_age <- cens_ind %>% group_by(factor(cens_ind$Age.group))%>%summarise(lit_rate = cens_ind$LP/cens_ind$TP)

lit_rate <- cens_ind$LP/cens_ind$TP
cens_ind <- cbind(cens_ind,lit_rate)
cens <- cbind(cens,lit_rate)

