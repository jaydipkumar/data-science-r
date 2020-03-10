library(arules)
library(arulesViz)

Movies <- read.csv("~/Downloads/Data Science/data set/my_movies.csv")

attach(Movies)
head(Movies)
Movies <- Movies[,6:15]

Movies_rule <- apriori(as.matrix(Movies))

inspect(sort(head(Movies_rule)),by="lift")
str(Movies_rule)

Movies_rule_1 <- apriori(as.matrix(Movies),parameter = list(support=0.1,confidence=0.8,minlen=5))
inspect(sort(head(Movies_rule_1)),by="lift")

plot(Movies_rule_1,methods="matrix")
plot(Movies_rule_1,methods="graph")
plot(Movies_rule_1,methods="paracoord")

Movies_Sub_rule <- subset(Movies_rule_1,lift>4)
plot(Movies_Sub_rule, method="matrix", measure="lift")
plot(Movies_Sub_rule, method="matrix3D", measure="lift")
plot(Movies_Sub_rule, method="grouped")
plot(Movies_Sub_rule, method="paracoord")






