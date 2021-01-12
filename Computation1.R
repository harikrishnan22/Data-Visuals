library(tidyr)
library(magrittr)
library(dplyr)
library(igraph)

#Adjacency Matrix
articles <- read.csv("c:/Computation and Vizualization/Assignment 6/Keyword_data.csv", header = TRUE, sep = ",", check.names = FALSE, na.strings = "")

articles <- articles[,-1]

articles <- as.data.frame(lapply(articles, toupper))

s <- stack(articles)

u <- unique(s$values)

adjmat <- matrix(0, nrow = length(u), ncol = length(u))

colnames(adjmat) <- u

rownames(adjmat) <- u

for(row in 1:nrow(articles)){
  pair <- combn(articles[row,],2)
  pair <- as.data.frame(pair)
  pair <- pair[,colSums(is.na(pair)) == 0]
  for(word in 1:ncol(pair)){
      first_row = which(rownames(adjmat) == pair[1,word])
      second_col = which(colnames(adjmat) == pair[2,word])
      adjmat[first_row,second_col] = adjmat[first_row,second_col] + 1
      adjmat[second_col,first_row] = adjmat[second_col,first_row] + 1
  }
}

#Links
key1 <- c()
key2 <- c()
keycount <- c()

for(word_one in 1:ncol(adjmat)){
  for(word_two in word_one:nrow(adjmat)){
    key1 <- append(key1,colnames(adjmat)[word_one])
    key2 <- append(key2,rownames(adjmat)[word_two])
    keycount <- append(keycount, adjmat[word_one,word_two])
  }
}

keywords <- data.frame("Keyword_1" = key1, "Keyword_2" = key2, "Weight" = keycount)

keywords_links <- keywords %>% drop_na()

deleted_rows_1 <- c()

for(row in 1:nrow(keywords_links)){
  if(keywords_links[row,1] == keywords_links[row,2]){
    deleted_rows_1 <- append(deleted_rows_1, row)
  }
}

keywords_links <- keywords_links[-deleted_rows_1,]

rownames(keywords_links) <- 1:nrow(keywords_links)

keywords_links[keywords_links == 0] <- NA

keywords_links <- keywords_links %>% drop_na()

write.csv(keywords_links, "keyword_links.csv")

#Points
keywords_points_1 <- keywords_links %>% group_by(Keyword_1) %>% summarise(Size = n())

keywords_points_2 <- keywords_links %>% group_by(Keyword_2) %>% summarise(Size = n())

colnames(keywords_points_1)[1] <- "Keyword"

colnames(keywords_points_2)[1] <- "Keyword"

keywords_points <- rbind(keywords_points_1,keywords_points_2)

keywords_points <- keywords_points %>% group_by(Keyword) %>% summarise(Degree = sum(Size))

write.csv(keywords_points, "keyword_points.csv")

top_n(keywords_points,3)

top_n(keywords_links,1)
