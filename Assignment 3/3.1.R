# 1. Create a barplot of cyl variable of mtcars data with and without using factor argument

# Without factor argument
barplot(table(mtcars$cyl), xlab="Cars CYL", ylab="Frequency", main="Barplot of CYL without factor")

# With factor argument
plot(as.factor(mtcars$cyl), xlab="Cars CYL", ylab="Frequency", main="Barplot(plot) of CYL with factor")

# 2. Locate median of mpg variable of mtcars data graphically and compare the median value with in-built function of R

hist(mtcars$mpg, main="Histogram of MPG with median")
abline(v=median(mtcars$mpg), lwd=3, lty=2, col="red")
legend("topright", legend="Median", col="red", lwd=3)
axis(side = 1, at = seq(min(mtcars$mpg), max(mtcars$mpg), by = 1), tck = -0.01, labels = FALSE)

# Median value of mpg from in-built function of R
median(mtcars$mpg)

# We can the same value from graph & from median function i.e. 19.2


# 3. Locate mode of mpg variable of mtcars data graphically and compare the mode value with the in-built/custom function of R

mode <- 3*median(mtcars$mpg) - 2*mean(mtcars$mpg)
mode

hist(mtcars$mpg, main="Histogram of MPG with mode")
abline(v=mode, lwd=3, lty=2, col="green")
axis(side = 1, at = seq(min(mtcars$mpg), max(mtcars$mpg), by = 1), tck = -0.01, labels = FALSE)
legend("topright", legend = "Mode", col= "green", lwd=3)

#From graph, we can notice mode falls in approx 17.5 and we have the calculated mode = 17.41875 

# 4. Create a scatterplot of mgp (dependent) and wt (independent) variable of mtcars data, add ablines at mean plus-minus 2 standard deviation and show the name of the cars with mpg > 2 standard deviation from mean inside the scatterplot obtained above


plot(mtcars$wt, mtcars$mpg, ylim=c(0,40), main = "Scatterplot of MPG and Weight", xlab = "Weight", ylab = "Miles per Gallon")

upper_lim <- mean(mtcars$mpg)+2*sd(mtcars$mpg)
lower_lim <- mean(mtcars$mpg)-2*sd(mtcars$mpg)

abline(h=upper_lim, lwd=2, lty=2, col="red")
abline(h=lower_lim, lwd=2, lty=2, col="red")

#search cars with greater than upper_limit value & then use rownames() to get name of cars
text(mtcars$wt[mtcars$mpg > upper_lim], mtcars$mpg[mtcars$mpg > upper_lim], rownames(mtcars)[mtcars$mpg > upper_lim], cex = 0.6, pos=4, col="blue")


# 5. Create a x variable with 100 random numbers and y variable with x + 100 random numbers; create a factor variable with 100 monthly random observations, create a time series data with 100 random values starting from January 1970; create a date variable with 100 random values starting from 1970/01/01 increasing each day; create a new variable z with square of x variable

set.seed(16)
# variable with 100 random numbers
x <- sample(100, replace = TRUE)

#variable with x + 100 random numbers
y <- x + sample(100, replace = TRUE)

# factor variable with 100 random month observation
month <- factor(sample(month.name, 100, replace = TRUE))

#time series data starting from 1970 January
ts_data <- ts(sample(100, replace = TRUE), start = c(1970, 1), frequency = 12)

# Print the time series object
ts_data

# date variable with 100 random values starting from 1970/01/01
date <- as.Date("1970/01/01") + sample(0:365, 100, replace = TRUE)
date

z <- x^2

# 6. Create a 2 x 3 plot window with scatterplot, barplot, boxplot, time series plot, date plot and square function plot

par(mfrow = c(2,3))

# scatter plot of x and y
plot(x, y, main = "Scatterplot of x and y", xlab = "x", ylab = "y")

# bar plot of factor variable month
barplot(table(month), main = "Barplot of month", xlab = "Month", ylab = "Frequency")

# box plot of ts_data
boxplot(ts_data, main = "Boxplot of ts_data")

# time series plot of ts_data
plot(ts_data, main = "Time series plot of ts_data", xlab = "Year", ylab = "Value")

# date plot of date and y
plot(date, y, main = "Date plot of date and y", xlab = "Date", ylab = "y")

# plot of z vs x
plot(x, z, main = "Plot of z vs x", xlab = "x", ylab = "z")


par(mfrow = c(1,1))

logx <- log(x)
logz <- log(z)

plot(logx, logz, main = "Plot of log x and log z", xlab = "log(x)", ylab = "log(z)")
# It shows the linear relation between logx & logz

# 8. Create a 1x1 plot window with correlation matrix plot of first three numerical variable of mtcars data (cyl is not numeric, its factor!)


# remove cyl variable 
mtcars_sub <- select(mtcars, -c(cyl))
# get first three numerical variables
mtcars_sub <- mtcars_sub[, 1:3]

# create a correlation matrix
cars_corr <- cor(mtcars_sub)

library(corrplot)

# plot correlation matrix
corrplot(cars_corr, method = "circle")
