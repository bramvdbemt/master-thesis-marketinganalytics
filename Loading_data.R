# Loading packages
library(dplyr)
library(tidyr)
library(tidyverse)
library(data.table)

# Reading the data

Amazon_with_dates <- data.table(readRDS('src/labeled_am_with_dates.Rds'))
Goodread_with_dates <- data.table(readRDS('src/labeled_gr_with_dates.Rds'))
Amazon_rev_overlap <- data.table(readRDS('src/amazon_rev_overlap.Rds'))

# Descriptive statistics for rating
mean(Amazon_rev_overlap$overall)
summary(Amazon_rev_overlap)
sd(Amazon_rev_overlap$overall)
nrow(Amazon_rev_overlap)

# Convert timestamps entire dataset

Amazon_rev_overlap_timestamps <- Amazon_rev_overlap %>% separate(reviewTime, c('month', 'day', 'year'))
Amazon_rev_overlap_timestamps$date <- paste(Amazon_rev_overlap_timestamps$day, Amazon_rev_overlap_timestamps$month, sep = '-')
Amazon_rev_overlap_timestamps$date <- paste(Amazon_rev_overlap_timestamps$date, Amazon_rev_overlap_timestamps$year, sep = '-')

# Draw a sample and split column of reviewTime to separate columns per month, day and year

sample_df <- Amazon_rev_overlap[sample(nrow(Amazon_rev_overlap), size = 10000, replace = FALSE), ]
View(sample_df)

sample_df <- sample_df %>% separate(reviewTime, c('month', 'day', 'year'))
sample_df$date <- paste(sample_df$year, sample_df$month, sep = "-")
sample_df$date <- paste(sample_df$date, sample_df$day, sep = "-")

# filter sample date
sample_df_before <- sample_df %>% filter(sample_df$date < '2013-03-18')
sample_df_after <- sample_df %>% filter(sample_df$date > '2013-03-18')

mean(sample_df_before$overall)
mean(sample_df_after$overall)
sd(sample_df_before$overall)
sd(sample_df_after$overall)
nrow(sample_df_before)
nrow(sample_df_after)

p <- ggplot(sample_df, aes(x=date, y=reviewerID)) +
    geom_line()
p

