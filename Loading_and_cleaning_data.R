# Installing packages
install.packages("plm")

# Loading packages
library(dplyr)
library(tidyr)
library(tidyverse)
library(data.table)
library(ggplot2)
library(lubridate)
library(fixest)
library(modelsummary)
library(mediation)
library(plm)

# Reading the data #----
Amazon_reviews <- read.csv('src/amazon_reviews.csv')
Goodreads_reviews <- read.csv('src/goodreads_reviews.csv')

# Descriptive statistics for rating
mean(Amazon_reviews$overall)
mean(Goodreads_reviews$rating)

## Amazon #----

# Compute dates Amazon Reviews
Amazon_reviews$date <- as.Date(as.POSIXct(Amazon_reviews$unixReviewTime, origin = "1970-01-01"))

# Rename variables
Amazon_reviews$rating <- Amazon_reviews$overall

# Drop variables that are not used
Amazon_df <- subset(Amazon_reviews, select = c(rating, date, asin))

# Create dummy variable for acquisition or not
Amazon_df$acquisition <- ifelse(Amazon_df$date < '2013-03-18', 0, 1)

# Summary statistics Amazon
summary(Amazon_df)
sd(Amazon_df$rating)

# Amazon before and after acquisition
Amazon_before_acquisition <- Amazon_df %>% filter(Amazon_df$date < '2013-03-18')
Amazon_after_acquisition <- Amazon_df %>% filter(Amazon_df$date >= '2013-03-18')

# Summary statistics before and after acquisition
summary(Amazon_before_acquisition)
summary(Amazon_after_acquisition)
sd(Amazon_before_acquisition$rating)
sd(Amazon_after_acquisition$rating)

# Visualization of Volume Amazon #----
# Use quarter for thesis
counts_Amazon_df <- Amazon_df %>% group_by(date) %>% summarise(n_reviews=n())

counts_quarter_Amazon_df = Amazon_df %>% 
    mutate(quarter = quarter(date), year = year(date)) %>% 
    group_by(quarter, year) %>% 
    summarise(n_reviews=n(), date = min(date)) %>% 
    arrange(date)

counts_month_Amazon_df = Amazon_df %>% 
    mutate(month = month(date), year = year(date)) %>% 
    group_by(month, year) %>% 
    summarise(n_reviews=n(), date = min(date)) %>% 
    arrange(date)

ggplot(data=counts_Amazon_df) +
    geom_line(aes(x=date, y=n_reviews)) +
    geom_vline(xintercept = as.numeric(as.Date("2013-03-18")), linetype=4, color ="red") +
    ggtitle("Amazon: Number of reviews per date")

ggplot(data=counts_quarter_Amazon_df) +
    geom_line(aes(x=date, y=n_reviews)) +
    geom_vline(xintercept = as.numeric(as.Date("2013-03-18")), linetype=4, color = "red") +
    ggtitle("Amazon: Number of reviews per quarter") +
    xlab("Time") + 
    ylab("Number of reviews")

ggplot(data=counts_month_Amazon_df) +
    geom_line(aes(x=date, y = n_reviews)) +
    geom_vline(xintercept = as.numeric(as.Date("2013-03-18")), linetype=4, color ="red") +
    ggtitle("Amazon: Number of reviews per month") +
    xlab("Time") + 
    ylab("Number of reviews")

# Visualization of Valence Amazon #----
rating_Amazon_df <- Amazon_df %>% group_by(date) %>% summarise(mean_rating = mean(rating))

avg_per_quarter_Amazon_df = Amazon_df %>% 
    mutate(quarter = quarter(date), year = year(date)) %>% 
    group_by(quarter, year) %>% 
    summarise(mean_rating = mean(rating), date = min(date)) %>% 
    arrange(date)

avg_per_month_Amazon_df = Amazon_df %>% 
    mutate(month = month(date), year = year(date)) %>% 
    group_by(month, year) %>% 
    summarise(mean_rating = mean(rating), date = min(date)) %>% 
    arrange(date)

ggplot(data=rating_Amazon_df) +
    geom_line(aes(x=date, y=mean_rating)) +
    geom_vline(xintercept = as.numeric(as.Date("2013-03-18")), linetype=4, color ="red") +
    ggtitle("Amazon: Average rating per date")

ggplot(data=avg_per_quarter_Amazon_df) +
    geom_line(aes(x=date, y=mean_rating)) +
    geom_vline(xintercept = as.numeric(as.Date("2013-03-18")), linetype=4, color = "red") +
    ggtitle("Amazon: Average rating per quarter")

ggplot(data=avg_per_month_Amazon_df) +
    geom_line(aes(x=date, y = mean_rating)) +
    geom_vline(xintercept = as.numeric(as.Date("2013-03-18")), linetype=4, color ="red") +
    ggtitle("Amazon: Average rating over per month")

# Analysis Amazon #----
Amazon_model_1 <- feols(rating ~ acquisition
                        |
                            asin,
                        data = Amazon_df)
Amazon_model_1

## Goodreads #----

# Compute dates Goodreads Reviews
Goodreads_reviews <- Goodreads_reviews %>% separate(date_updated, c('weekday', 'month', 'day', 'hour', 'minute', 'second', 'timezone', 'year'))
Goodreads_reviews = Goodreads_reviews %>%
    mutate(Monthnumber = recode(Goodreads_reviews$month,
                               "Jan" = 01,
                               "Feb" = 02,
                               "Mar" = 03,
                               "Apr" = 04,
                               "May" = 05,
                               "Jun" = 06,
                               "Jul" = 07,
                               "Aug" = 08,
                               "Sep" = 09,
                               "Oct" = 10,
                               "Nov" = 11,
                               "Dec" = 12))
Goodreads_reviews$date <- paste(Goodreads_reviews$year, Goodreads_reviews$Monthnumber, sep="-")
Goodreads_reviews$date <- paste(Goodreads_reviews$date, Goodreads_reviews$day, sep="-")
Goodreads_reviews$date <- as.Date(Goodreads_reviews$date)

# Drop variables that are not used
Goodreads_df <- subset(Goodreads_reviews, select = c(rating, date, book_id))

# Create dummy variable for acquisition or not
Goodreads_df$acquisition <- ifelse(Goodreads_df$date < '2013-03-18', 0, 1)

# Summary statistics Goodreads
summary(Goodreads_df)
sd(Goodreads_df$rating)

# Create subsets before and after acquisition
Goodreads_before_acquisition <- Goodreads_df %>% filter(Goodreads_df$date < '2013-03-18')
Goodreads_after_acquisition <- Goodreads_df %>% filter(Goodreads_df$date >= '2013-03-18')

# Summary before and after acquisition
summary(Goodreads_before_acquisition)
summary(Goodreads_after_acquisition)
sd(Goodreads_before_acquisition$rating)
sd(Goodreads_after_acquisition$rating)

# Visualization of volume Goodreads #----
# Use quarter for thesis
counts_Goodreads_df <- Goodreads_df %>% group_by(date) %>% summarise(n_reviews=n())

counts_quarter_Goodreads_df = Goodreads_df %>% 
    mutate(quarter = quarter(date), year = year(date)) %>% 
    group_by(quarter, year) %>% 
    summarise(n_reviews=n(), date = min(date)) %>% 
    arrange(date)

counts_month_Goodreads_df = Goodreads_df %>% 
    mutate(month = month(date), year = year(date)) %>% 
    group_by(month, year) %>% 
    summarise(n_reviews=n(), date = min(date)) %>% 
    arrange(date)

ggplot(data=counts_Goodreads_df) +
    geom_line(aes(x=date, y=n_reviews)) +
    geom_vline(xintercept = as.numeric(as.Date("2013-03-18")), linetype=4, color ="red") +
    ggtitle("Goodreads: Number of reviews per date")

ggplot(data=counts_quarter_Goodreads_df) +
    geom_line(aes(x=date, y=n_reviews)) +
    geom_vline(xintercept = as.numeric(as.Date("2013-03-18")), linetype=4, color = "red") +
    ggtitle("Goodreads: Number of reviews per quarter") +
    xlab("Time") + 
    ylab("Number of reviews")

ggplot(data=counts_month_Goodreads_df) +
    geom_line(aes(x=date, y = n_reviews)) +
    geom_vline(xintercept = as.numeric(as.Date("2013-03-18")), linetype=4, color ="red") +
    ggtitle("Goodreads: Number of reviews per month") +
    xlab("Time") + 
    ylab("Number of reviews")

# Visualization of valence Goodreads #----
# Use quarter for thesis
rating_Goodreads_df <- Goodreads_df %>% group_by(date) %>% summarise(mean_rating = mean(rating))

avg_per_quarter_Goodreads_df = Goodreads_df %>% 
    mutate(quarter = quarter(date), year = year(date)) %>% 
    group_by(quarter, year) %>% 
    summarise(mean_rating = mean(rating), date = min(date)) %>% 
    arrange(date)

avg_per_month_Goodreads_df = Goodreads_df %>% 
    mutate(month = month(date), year = year(date)) %>% 
    group_by(month, year) %>% 
    summarise(mean_rating = mean(rating), date = min(date)) %>% 
    arrange(date)

ggplot(data=rating_Goodreads_df) +
    geom_line(aes(x=date, y=mean_rating)) +
    geom_vline(xintercept = as.numeric(as.Date("2013-03-18")), linetype=4, color ="red") +
    ggtitle("Goodreads: Average rating per date")

ggplot(data=avg_per_quarter_Goodreads_df) +
    geom_line(aes(x=date, y=mean_rating)) +
    geom_vline(xintercept = as.numeric(as.Date("2013-03-18")), linetype=4, color = "red") +
    ggtitle("Goodreads: Average rating per quarter")

ggplot(data=avg_per_month_Goodreads_df) +
    geom_line(aes(x=date, y = mean_rating)) +
    geom_vline(xintercept = as.numeric(as.Date("2013-03-18")), linetype=4, color ="red") +
    ggtitle("Goodreads: Average rating over per month")





# Analysis Goodreads #----
# Effect of X on Y
Goodreads_model_1 <- feols(rating ~ acquisition
              |
                book_id,
              data = Goodreads_df)
summary(model_1)


# Effect of X on M
model_2 <- feols(n(rating) ~ acquisition
                 |
                     book_id,
                 data = Goodreads_df)


