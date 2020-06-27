
groceries<- read.csv(file.choose())
View(groceries)

library(arules)
library(arulesViz)

#groc<-read.transactions(groceries, format = "basket")
#groc<-read.transactions(groceries, format = "basket", encoding = "unknown")
groc<- read.transactions(file.choose(new=FALSE), format = "basket")
inspect(groc)
class(groc)
itemFrequencyPlot(groc, topN=20)

# apply apriori algo

groc_rules<- apriori(groc, parameter = list(support= 0.002, confidence=0.05, minlen= 2))
inspect(groc_rules)
inspect(groc_rules[1:10])

plot(groc_rules, method = "graph")                     
plot(groc_rules, method = "scatterplot")                     
plot(groc_rules, method = "grouped")                     

rules<- sort(groc_rules, by="lift")
inspect(rules)
inspect(rules[1:5])

rules_1 <- sort(groc_rules, by="confidence")
inspect(rules_1)
inspect(rules_1[1:5])

is.redundant(rules, measure="lift")

rules_2 <- rules[!is.redundant(rules, measure="lift")]
inspect(rules_2)
inspect(rules_2[1:5])

########

groceries_rules<- apriori(groc, parameter = list(support= 0.004, confidence=0.01, minlen= 3))
inspect(groceries_rules)

plot(groceries_rules, method = "graph")                     
plot(groceries_rules, method = "scatterplot")                     
plot(groceries_rules, method = "grouped")                     

gg_rules<- sort(groceries_rules, by="lift")
inspect(gg_rules)

gg_rules_1 <- sort(groceries_rules, by="confidence")
inspect(gg_rules_1)

is.redundant(gg_rules_1, measure="confidence")

g_rules_2 <- gg_rules_1[!is.redundant(gg_rules_1, measure="confidence")]
inspect(g_rules_2)
inspect(g_rules_2[1:5])

gg_rules_2 <- gg_rules[!is.redundant(gg_rules, measure="lift")]
inspect(gg_rules_2)
inspect(gg_rules_2[1:5])

#################

apriori_rules<- apriori(groc, parameter = list(support= 0.003, confidence=0.08, minlen= 2))
inspect(apriori_rules)

plot(apriori_rules, method = "graph")                     
plot(apriori_rules, method = "scatterplot")                     
plot(apriori_rules, method = "grouped")                     

ap_rules<- sort(apriori_rules, by="lift")
inspect(ap_rules)

ap_rules_1 <- sort(apriori_rules, by="confidence")
inspect(ap_rules_1)

is.redundant(ap_rules_1, measure="confidence")

ap_rules_2 <- ap_rules_1[!is.redundant(ap_rules_1, measure="confidence")]
inspect(ap_rules_2)
inspect(ap_rules_2[1:5])

apri_rules_2 <- ap_rules[!is.redundant(ap_rules, measure="lift")]
inspect(apri_rules_2)
inspect(apri_rules_2[1:5])
