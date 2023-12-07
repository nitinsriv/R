library(ggplot2)

## Importing the dataset

main <- read.csv("C:\\IIITB\\clustering\\R\\Main.csv", stringsAsFactors=FALSE)
main_il <- main[,c(1,2)]

main_il$Illiterate <- scale(main_il$Illiterate,center = FALSE, scale=TRUE)

distM <- dist(main_il)
hcl1 <- hclust(distM,method="single")
plot(hcl1)

hcl2 <- hclust(distM,method="complete")
plot(hcl2)

hcl3 <- hclust(distM,method="average")
plot(hcl3)

rect.hclust(hcl2, k=5, border="red")
clusterCut <- cutree(hcl2, k=5)

main <- cbind(main,clusterCut)
colnames(main)[7] <- "ClusterId_il"


main_per <- main[,c(1,5,6)]
main_per$Percentage.Illiterate <- scale(main_per$Percentage.Illiterate,center = FALSE, scale=TRUE)
main_per$Percentage.Graduate...above <- scale(main_per$Percentage.Graduate...above,center = FALSE, scale=TRUE)


distM_per <- dist(main_per)
hcl1_per <- hclust(distM_per,method="complete")
plot(hcl1_per)
rect.hclust(hcl1_per, k=5, border="red")
clusterCut_per <- cutree(hcl1_per, k=5)


hcl2_per <- hclust(distM_per,method="single")
plot(hcl2_per)

hcl3_per <- hclust(distM_per,method="average")
plot(hcl3_per)

main <- cbind(main,clusterCut_per)
colnames(main)[8] <- "ClusterId_per"


main_per_il <- main[,c(1,2,5,6)]
main_per_il$Percentage.Illiterate <- scale(main_per$Percentage.Illiterate,center = FALSE, scale=TRUE)
main_per_il$Percentage.Graduate...above <- scale(main_per$Percentage.Graduate...above,center = FALSE, scale=TRUE)
main_per_il$Illiterate <- scale(main_per_il$Illiterate,center = FALSE, scale=TRUE)


distM_per_il <- dist(main_per_il)
hcl1_per_il <- hclust(distM_per_il,method="complete")
plot(hcl1_per_il)
rect.hclust(hcl1_per_il, k=5, border="red")
clusterCut_per_il <- cutree(hcl1_per_il, k=5)


hcl2_per_il <- hclust(distM_per_il,method="single")
plot(hcl2_per_il)

hcl3_per_il <- hclust(distM_per_il,method="average")
plot(hcl3_per_il)

main <- cbind(main,clusterCut_per_il)
colnames(main)[9] <- "ClusterId_il_per"