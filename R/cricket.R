library(dplyr)

cric <- read.csv("C:\\IIITB\\clustering\\R\\Cricket.csv")

cric$SR <- scale(cric$SR,center = FALSE, scale = TRUE)
cric$Ave <- scale(cric$Ave, center = FALSE, scale = TRUE)

cric_clus <- cric[,c(8,10)]

r_sq <- rnorm(20)
for (number in 1:20){
  clus <- kmeans(cric_clus, centers = number, nstart = 50)
  r_sq[number]<- clus$betweenss/clus$totss
}

plot(r_sq)

clus_0 <- kmeans(cric_clus, centers = 4, nstart = 50)
clus_1 <- kmeans(cric_clus, centers = 5, nstart = 50)
clus_2 <- kmeans(cric_clus, centers = 6, nstart = 50)
clus_3 <- kmeans(cric_clus, centers = 7, nstart = 50)
clus_4 <- kmeans(cric_clus, centers = 8, nstart = 50)

cric <- cbind(cric,clus_0$cluster)
colnames(cric)[14] <- "ClusterId"
tab <- table(cric$ClusterId)

player_4 <- cric[which(cric$ClusterId==4),]

cric <- cric %>% group_by(cric$ClusterId)


distM <- dist(cric_clus)

hcl_1 <- hclust(distM,method="complete")
plot(hcl_1)

## Visualising the cut in the dendrogram

rect.hclust(hcl_1, k=4, border="red")
clusterCut <- cutree(hcl_1, k=4)

cric <- cbind(cric,clusterCut)
