library(arules)
library(arulesViz)

Groceries <- read.transactions("~/Downloads/Data Science/data set/groceries.csv",sep = ",")
Groceries_New <-read.transactions("~/Downloads/Data Science/data set/groceries.csv",format="basket")
inspect(head(Groceries_New))
inspect(head(Groceries))
summary(Groceries_New)
summary(Groceries)

itemFrequencyPlot(Groceries,topN=20)

#Rules

Groceries_rules_1 <- apriori(Groceries,parameter = list(support=0.001,confidence=0.8,minlen=1))
inspect(head(Groceries_rules_1))
inspect(sort(head(Groceries_rules_1),by="lift"))

Groceries_rules_2 <- apriori(Groceries,parameter = list(support=0.001,confidence=0.8,minlen=1))
inspect(head(Groceries_rules_2))
inspect(sort(head(Groceries_rules_2),by="lift"))

plot(Groceries_rules_2,methods="matrix")                        
plot(Groceries_rules_2,method="graph")
plot(Groceries_rules_2,method="paracoord")

Groceries_sub <- subset(Groceries_rules_2,lift >4)
plot(Groceries_rules_2, method="matrix", measure="lift")
plot(Groceries_rules_2, method="matrix3D", measure="lift")
plot(Groceries_rules_2, method="grouped")
plot(Groceries_rules_2, method="paracoord")

