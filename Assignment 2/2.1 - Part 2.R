setwd("C:/Users/user/Documents/imdb")
library(tidyverse)
# 8: Import title.ratings.tsv as df_ratings
df_ratings <- read_tsv("title.ratings.tsv")

# 9: Check structure and summary & interpret
str(df_ratings)
summary(df_ratings)
#from structure, we can observer that there are 3 columns named tconst which is character, averageRating is numeric & numVotes is also numeric
#In Summary for numVotes, 3rd Quartile is 101 but the Max is 2726020  which denotes there is a outlier.

boxplot(df_ratings$averageRating)
boxplot(df_ratings$numVotes)
hist(df_ratings$averageRating)
#In summary for averageRating, there is extreme outliers as seen in the boxplot and skewed to left i.e negative skewness as seen in histogram



# 10: Is this dataset tidy?

head(df_ratings)
any(is.na(df_ratings))

# Yes, the dataset is tidy.
# By checking head, we can see that every variable has it's own column & using is.na() we can check if any value is missing or not.
# As there is not any missing value which means every row & column has value & every variable has it's own cell, so we can say that the data is tidy.


# 11: Create scatterplot of averageRating vs numVotes

library(ggplot2)

ggplot(df_ratings, aes(x = numVotes, y = averageRating)) + 
  geom_point() +
  xlab("Number of Votes") +
  ylab("Average Rating") +
  ggtitle("Scatterplot of Average Rating vs Number of Votes")

# We can interpret from scatterplot that there is positive correlation i.e. as number of votes increases, average rating also increases


# 12: Import title.basics.tsv as df_basics
df_basics <- read_tsv("title.basics.tsv")
head(df_basics)


# 13: Check the structure and get summary of all the variables and interpret them carefully
str(df_basics)
summary(df_basics)

summary(df_basics$isAdult)
#From str, we can observe that most of the columns are of character type & only isAdult is of double type. We can convert character type with numeric using as.numeric
#Also, we can only get the summary for isAdult where the minimum, Q1 & median is 0, also the max is 2023 which seems to be data entry error. 
#There is also one row with NA value in isAdult as seen in summary.


# 14: Is this dataset tidy?
#No, this dataset is also not tidy as it contains multiple values in the "genres" column. To be tidy, each value should have its own column. Some of the numeric columns are also showing as character.

# Modifying numerical columns to be numeric from character type
df_basics <- df_basics %>%
  mutate(startYear = as.numeric(startYear),
         endYear = as.numeric(endYear),
         runtimeMinutes = as.numeric(runtimeMinutes))


# 15: Join "df_basics" variables in "df_ratings" using "left_join"
library(dplyr)

df_ratings <- left_join(df_ratings, df_basics, by = "tconst")
head(df_ratings)
str(df_ratings)

# 16: Get scatterplot of runtimeMinutes and averageRating variable and interpret then carefully
ggplot(df_ratings, aes(x = runtimeMinutes, y = averageRating)) +
         geom_point(alpha = 0.5)+
         labs(title = "Scatter plot of runtimeMinutes & averageRating",
              x= "Runtime (in Min)", y = "Average Ratings")
       




# 17: Get frequency table of "genres" variable using "plyr" package, if required and interpret it carefully


# Split the "genres" variable and unnest (separate row after splitting) the resulting list column, as there are multiple values for some row
genres_freq <- df_ratings %>% 
  filter(!is.na(genres)) %>% 
  mutate(genres = strsplit(genres, ",")) %>% 
  unnest(genres) %>% 
  count(genres, sort = TRUE)

genres_freq


# 18: Create a "by_genres" object using group_by function with df_ratings data and genres variable
by_genres <- df_ratings %>% 
  filter(!is.na(genres)) %>% 
  group_by(genres)

head(by_genres)

# 19: Get mean of runtimeMinutes variables using summarise function by genres, remove NA in runtimeMinutes if required

mean_runtimes <- summarise(by_genres, mean_runtime = mean(runtimeMinutes, na.rm = TRUE))
mean_runtimes

head(df_ratings)

# 20: Get mean of runtimeMinutes variables using summarise by genres without creating "by_genres" object 


mean_runtimes <- df_ratings %>% 
  filter(!is.na(genres)) %>% 
  group_by(genres) %>% 
  summarise(mean_runtime = mean(runtimeMinutes, na.rm = TRUE))

mean_runtimes

# 21: What is difference between step 19 and step 20? Which one do you prefer? Why? Interpret the result of your choice.

# The difference in step 19 & step 20 is that in step 19, by_genres object is created & then used for summarizing runtimeMinutes.
# For summarizing, I will prefer step 20 as that won't create additional object & won't consume extra memory. But if there will be multiple operations using by_genres, then I will prefer step 19.


# 22: Filter "df_ratings" data with runtimeMinutes less than 150 minutes and save it as "df_ratings_movie150m"

df_ratings_movie150m <- df_ratings %>% filter(runtimeMinutes < 150 & !is.na(runtimeMinutes))
tail(df_ratings_movie150m)


# 23: Get scatterplot of runtimeMinutes and averageRating variable for this new data and interpret then carefully
ggplot(na.omit(df_ratings_movie150m), aes(x = runtimeMinutes, y = averageRating)) + 
  geom_point(alpha = 0.5) + 
  labs(x = "Runtime (minutes)", y = "Average rating")

# From scatterplot we can observe that there are movies with runtime less than 60, also such movies with less runtime have higher ratings


# 24: Arrange the df_rating_movie150m data in descending order by averageRating and save it as "best_worst_movies"
best_worst_movies <- df_ratings_movie150m %>%
  arrange(desc(averageRating))

# 25: Show the top 6 and last 6 movies based on the arranged dataset above 
head(best_worst_movies, 6)
tail(best_worst_movies, 6)

# 26: Get the averageRating of adult movies (isAdult variable) using mutate function and interpret it carefully

adult_avg_rating <- df_ratings %>% mutate(isAdult = as.logical(isAdult)) %>%
                    filter(isAdult) %>%
                    summarise(avg_rating = mean(averageRating, na.rm = TRUE))

adult_avg_rating

# 27: Divide the "df_ratings_movies150m" into training and testing dataset with 80% and 20% split with slice function

set.seed(123)
df_train <- df_ratings_movie150m %>% slice_sample(prop = 0.8)
df_test <- df_ratings_movie150m %>% slice_sample(prop = 0.2)

df_train
df_test


# 28: Get mean, standard deviation, median and interquartile range of averageRating, numVotes and runtimeMinutes 
# variable of training and testing data and interpret them carefully

train_summary <- df_train %>% 
  summarize(mean_rating = mean(averageRating),
            sd_rating = sd(averageRating),
            median_rating = median(averageRating),
            iqr_rating = IQR(averageRating),
            mean_votes = mean(numVotes),
            sd_votes = sd(numVotes),
            median_votes = median(numVotes),
            iqr_votes = IQR(numVotes),
            mean_runtime = mean(runtimeMinutes, na.rm = TRUE),
            sd_runtime = sd(runtimeMinutes, na.rm = TRUE),
            median_runtime = median(runtimeMinutes, na.rm = TRUE),
            iqr_runtime = IQR(runtimeMinutes, na.rm = TRUE))

test_summary <- df_test %>% 
  summarise(mean_rating = mean(averageRating),
            sd_rating = sd(averageRating),
            median_rating = median(averageRating),
            IQR_rating = IQR(averageRating),
            mean_votes = mean(numVotes),
            sd_votes = sd(numVotes),
            median_votes = median(numVotes),
            IQR_votes = IQR(numVotes),
            mean_runtime = mean(runtimeMinutes, na.rm = TRUE),
            sd_runtime = sd(runtimeMinutes, na.rm = TRUE),
            median_runtime = median(runtimeMinutes, na.rm = TRUE),
            IQR_runtime = IQR(runtimeMinutes, na.rm = TRUE))


train_summary
test_summary


# 29: Get histogram of averageRating, numVotes and runtimeMinutes variables of training and testing data; compare them and interpret them carefully

# Histograms for training data
hist(df_train$averageRating, main = "Average Rating - Training", xlab = "Rating")
hist(log(df_train$numVotes), main = "Number of Votes - Training", xlab = "Votes")
hist(df_train$runtimeMinutes, main = "Runtime - Training", xlab = "Minutes")


# Histograms for testing data
hist(df_test$averageRating, main = "Average Rating - Testing", xlab = "Rating")
hist(log(df_test$numVotes), main = "Number of Votes - Testing", xlab = "Votes")
hist(df_test$runtimeMinutes, main = "Runtime - Testing", xlab = "Minutes")


# 30: Get boxplot of averageRating, numVotes and runtimeMinutes variables of training and testing data; compare them and interpret them carefully


# Boxplot for training data
boxplot(df_train$averageRating, main = "Training Data: Average Rating", ylab = "Average Rating")
boxplot(df_train$numVotes, main = "Training Data: Number of Votes", ylab = "Number of Votes")
boxplot(df_train$runtimeMinutes, main = "Training Data: Runtime (minutes)", ylab = "Runtime")

# Boxplot for testing data
boxplot(df_test$averageRating, main = "Testing Data: Average Rating", ylab = "Average Rating")
boxplot(df_test$numVotes, main = "Testing Data: Number of Votes", ylab = "Number of Votes")
boxplot(df_test$runtimeMinutes, main = "Testing Data: Runtime (minutes)", ylab = "Runtime")
