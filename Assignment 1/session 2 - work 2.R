library(readr)

csv <- read.csv("D:/R programming runs/Assisgnments/Assignment 1/Manish Shah - covnep_252days.csv")


#data frame of date & total cases
df <- data.frame(csv$date, csv$totalCases)

#converting date column to date object
df$date <- as.Date(df$csv.date, "%m/%d/%Y")

plot(df$date, df$csv.totalCases)

summary(csv$totalCases)
#minimum value can't be 0 so adding 1 to totalCases

# Replace 0 with 1 in totalCases
csv$totalCases[csv$totalCases == 0] <- 1

# Check summary again
summary(csv$totalCases)


#newCases histogram
hist(csv$newCases)

summary(csv$newCases)
#the minimum value can't be 0 here also

#outlier can be seen in boxplot
boxplot(csv$newCases)
