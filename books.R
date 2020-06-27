book<- read.csv(file.choose())
View(book)

library(arules)
library(arulesViz)
str(book)
book[]<- lapply(book,as.matrix)
View(book)
barplot(sapply(book,sum),col=1:11)

#apply apriori algo
book_rules<- apriori(as.matrix(book), parameter = list(confidence= 0.5, support= 0.002, minlen= 2))
inspect(book_rules)                     

plot(book_rules)
plot(book_rules, method="graph")
plot(book_rules, method="grouped")

rules_1 <- sort(book_rules,by="confidence")
inspect(rules_1)
inspect(rules_1[1:10])

rules_2 <- sort(book_rules,by="lift")
inspect(rules_2[1:10])

is.redundant(rules_2, measure="lift")

rules_2 <- rules_2[!is.redundant(rules_2, measure="lift")]
inspect(rules_2)
inspect(rules_2[1:5])

###

a_rules<- apriori(as.matrix(book), parameter = list(confidence= 0.6, support= 0.006, minlen= 3))
inspect(a_rules)                     

plot(a_rules)
plot(a_rules, method="graph")
plot(a_rules, method="grouped")

a_rules_1 <- sort(a_rules,by="confidence")
inspect(a_rules_1)
inspect(a_rules_1[1:10])

a_rules_2 <- sort(a_rules,by="lift")
inspect(a_rules_2[1:10])

a_rules_3 <- sort(a_rules,by="count")
inspect(a_rules_3[1:10])

is.redundant(a_rules_1, measure="confidence")

a_rules_1 <- a_rules_1[!is.redundant(a_rules_1, measure="confidence")]
inspect(a_rules_1)
inspect(a_rules_1[1:5])

######

b_rules<- apriori(as.matrix(book), parameter = list(confidence= 0.4, support= 0.004, minlen= 3))
inspect(b_rules)                     

plot(b_rules)
plot(b_rules, method="graph")
plot(b_rules, method="grouped")

b_rules_1 <- sort(b_rules,by="confidence")
inspect(b_rules_1)
inspect(b_rules_1[1:10])

b_rules_2 <- sort(b_rules,by="lift")
inspect(b_rules_2[1:10])

b_rules_3 <- sort(b_rules,by="count")
inspect(b_rules_3[1:10])

?is.redundant

is.redundant(b_rules_1, measure="confidence")

bb_rules_1 <- b_rules_1[!is.redundant(b_rules_1, measure="confidence")]
inspect(bb_rules_1)
inspect(bb_rules_1[1:5])
