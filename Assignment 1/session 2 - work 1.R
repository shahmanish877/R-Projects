z <- c(1,1,2,2,2,2,2,3,3,3,3,3,3,3,3,3,3,3,4,4,4,4,4,5,5,5,6,6,7)
hist(z)
summary(z)

fivenum(z)

IQR(z)

#outlier in boxplot as 1, 6, 7 falls out of range [Q1-1.5*IQR, Q3+1.5*IQR]

boxplot(z)