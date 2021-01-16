library(ggplot2) #plot data
library(tidyr) #tidyverse
library(dplyr)

dat1 <- read.csv("input/result1.1.csv", stringsAsFactors = FALSE)
#dat1 <- dat1[order(article_cat_2),]


plotD1 <- ggplot(data = dat1 , aes(reorder(Article, desc(Quantity)), Quantity)) + 
  geom_bar(stat="identity") +
  xlab("Articles") +
  ylab("Sales Quantity") +
  theme(axis.text.x = element_text(angle = 45)) +
  labs(title = "Best Selling Products [Warm Weather]")

plotD1

dat2 <- read.csv("input/result1.2.csv", stringsAsFactors = FALSE)
#dat1 <- dat1[order(article_cat_2),]


plotD2 <- ggplot(data = dat2 , aes(reorder(Article, desc(Quantity)), Quantity)) + 
  geom_bar(stat="identity") +
  xlab("Articles") +
  ylab("Sales Quantity") +
  theme(axis.text.x = element_text(angle = 45)) +
  labs(title = "Best Selling Products [Cold Weather]")

plotD2
