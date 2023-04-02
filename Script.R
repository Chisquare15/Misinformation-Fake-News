#'========================================================
#----Data Preprocessing-----
#'========================================================

#loading library

library(tidyverse)
library(readr)
library(tm)
library(igraph)
library(tidytext)
library(sentimentr)
library(data.table)
library(ggplot2)
library(lessR)
library(wordcloud)


#setting up directory and loading data

setwd("~/CANIS hackathon")

data_fake = read.csv("DataSet_Misinfo_FAKE.csv",header = TRUE) %>%
  dplyr::mutate(label="fake")

data_true = read.csv("DataSet_Misinfo_TRUE.csv",header = TRUE)%>%
  dplyr::mutate(label="true")

# sentiment as per label


# The AFINN lexicon assigns words with a score that runs between -5 and 5,
# with negative scores indicating negative sentiment and positive scores indicating positive sentiment


afinn <- get_sentiments("afinn")

#  function to calculate sentiment 
calculate_sentiment <- function(text) {
  words <- tibble(word = str_split(text, "\\s+")[[1]])
  result <- inner_join(words, afinn, by = "word") %>%
    summarize(sentiment_score = sum(value))
  return(result$sentiment_score)
}

#apply the function to each row of the dataframe

data_fake$sentiment <- apply(data_fake, 1, function(x) calculate_sentiment(x["text"]))
data_true$sentiment <- apply(data_true, 1, function(x) calculate_sentiment(x["text"]))

#add sentiment category

data_fake<-data_fake %>%
  mutate(sentiment_category = ifelse(sentiment > 0, "Positive", "Negative")) 

data_true<-data_true %>%
  mutate(sentiment_category = ifelse(sentiment > 0, "Positive", "Negative")) 



#'========================================================
#----Visualization - Percentage of sentiment category-----
#'========================================================


per_data_true<-table(data_true$sentiment_category)

PieChart(per_data_true, hole = 0, values = "%", data = per_data_true,
         fill = c("lightblue", "pink"), main = "Distribution of sentiment category in true news")


#Comments- For true news we observe that there is a near equal distribution of sentiment


per_data_fake<-table(data_fake$sentiment_category)

PieChart(per_data_fake, hole = 0, values = "%", data = per_data_true,
         fill = c("lightblue", "pink"), main = "Distribution of sentiment category in fake news")


#Comments- For fake news we observe that there is a majority of negative sentiment being spread


