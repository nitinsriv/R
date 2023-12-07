
##-------------Bar Charts in ggplot--------------------

# setting the ggplot object

library(ggplot2)

# The base layer is available : cyl.am
cyl.am <- ggplot(mtcars,aes(x=factor(cyl)))
cyl.am+geom_bar()
cyl.am <- cyl.am + aes(fill = factor(am))
cyl.am+geom_bar()

# Add geom (position = "stack" by default)
cyl.am+geom_bar(position="stack")

# Fill - show proportion
cyl.am+geom_bar(position="fill")
cyl.am+geom_bar(fill="red")


ggplot(mtcars, aes(x = factor(cyl), fill =factor(am))) + geom_bar(alpha = 0.4, position = position_dodge(width=0.4))
