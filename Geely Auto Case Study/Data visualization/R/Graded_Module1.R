library(ggplot2)

ggplot(co,aes(conc,uptake,col=Type,size=uptake))+geom_point()

boxplot(uptake~conc,co)

ir <- iris

ggplot(iris,aes(ir$Species,ir$Petal.Length))+geom_boxplot()
ggplot(iris,aes(ir$Species,ir$Petal.Width))+geom_boxplot()

boxplot(iris,species~iris$Sepal.Length )