library(arules)
library(arulesViz)

book <- read.csv(file.choose())

attach(book)
str(book)
summary(book)

for (i in c(1:11)) {
  if (i == 1) {
    cat(" 0 "," 1 ","Colnames","\n")
  }
  cat(table(book[i])[[1]],
      table(book[i]) [[2]])
cat(" ",names(book)[i],"\n")
}

book_rules  <- apriori(as.matrix(book))
str(book_rules)
inspect(book_rules)

book_apro_2 <- apriori(as.matrix(book),parameter = list(support=0.02,confidence = 0.6,minlen=3))
inspect(sort(book_apro_2,by ="lift"))
summary(book_apro_2)

book_apro_3 <- apriori(as.matrix(book),parameter = list(support=0.03,confidence = 0.5,minlen=6))
inspect(sort(book_apro_3,by="lift"))
summary(book_apro_3)

plot(book_apro_2,methods="matrix")
plot(book_apro_2,methods="graph")
plot(book_apro_2,methods="paracoord")

book_sub_rules <- subset(book_apro_2,lift>4)
plot(book_sub_rules, method="matrix", measure="lift")
plot(book_sub_rules, method="matrix3D", measure="lift")
plot(book_sub_rules, method="grouped")
plot(book_sub_rules, method="paracoord")



