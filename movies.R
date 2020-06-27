movies<- read.csv(file.choose())
View(movies)
str(movies)

movie<- movies[,c(1:5)]
View(movie)

# converting into character
movie[]<- lapply(movie,as.character)
View(movie)
str(movie)

paste_movie <- function(i){
  return (paste(as.character(i),collapse=" "))
}

movie["new_col"] <- apply(movie,1,paste_movie)
View(movie)

install.packages("tm")
library(tm)


x <- Corpus(VectorSource(movie$new_col)) 
x <- tm_map(x,stripWhitespace)
# Creating a TDM matrix
db <- t(TermDocumentMatrix(x))
# Converting to data frame
db_df <- data.frame(as.matrix(db))
View(db_df)

#by replacing "db_df" to the "movie" in original database
movie_1<- movies[,c(6:11)]
View(movie_1)
movies_final<- merge(db_df,movie_1)
View(movies_final)

library(arules)
library(arulesViz)

str(movies_final)
movies_final[]<- lapply(movies_final,as.matrix)
View(movies_final)
barplot(sapply(movies_final,sum),col=1:19)

detach(package:tm, unload=TRUE)

#apply apriori algo
movies_rules<- apriori(as.matrix(movies_final), parameter = list(confidence= 0.4, support= 0.002, minlen= 2))
inspect(movies_rules)
inspect(movies_rules[1:10])

plot(movies_rules)
plot(movies_rules, method="graph")
plot(movies_rules, method="grouped")

rules <- sort(movies_rules,by="lift")
rules
inspect(rules)
inspect(rules[1:10])

rules_c <- sort(movies_rules,by="confidence")
inspect(rules_c[1:10])

is.redundant(rules_c, measure="confidence")

rules_c1 <- rules_c[!is.redundant(rules_c, measure="confidence")]
inspect(rules_c1)
inspect(rules_c1[1:5])

####

movie_rules<- apriori(as.matrix(movies_final), parameter = list(confidence= 0.6, support= 0.004, minlen= 3))
inspect(movie_rules)
inspect(movie_rules[1:10])

plot(movie_rules)
plot(movie_rules, method="graph")
plot(movie_rules, method="grouped")

m_rules <- sort(movie_rules,by="lift")
m_rules
inspect(m_rules)
inspect(m_rules[1:10])

m_rules_c <- sort(movies_rules,by="confidence")
inspect(m_rules_c[1:10])

is.redundant(m_rules, measure="lift")

rules_c2 <- m_rules[!is.redundant(m_rules, measure="lift")]
inspect(rules_c2)
inspect(rules_c2[1:5])

####

mov_rules<- apriori(as.matrix(movies_final), parameter = list(confidence= 0.8, support= 0.006, minlen= 2))
inspect(mov_rules)
inspect(mov_rules[1:10])

plot(mov_rules)
plot(mov_rules, method="graph")
plot(mov_rules, method="grouped")

mo_rules <- sort(mov_rules,by="lift")
mo_rules
inspect(mo_rules)
inspect(mo_rules[1:10])

mo_rules_c <- sort(mov_rules,by="confidence")
inspect(mo_rules_c[1:10])

is.redundant(mo_rules, measure="lift")

mo_rules_c2 <- mo_rules[!is.redundant(mo_rules, measure="lift")]
inspect(mo_rules_c2)
inspect(mo_rules_c2[1:5])

is.redundant(mo_rules_c, measure="confidence")

mo_rules_c3 <- mo_rules_c[!is.redundant(mo_rules_c, measure="confidence")]
inspect(mo_rules_c2)
inspect(mo_rules_c2[1:5])

#is.redundant(mov_rules)
