library(readxl)

excel_data <- read_excel("D:/R programming runs/Assisgnments/Assisgnment 1/Manish Shah - MR_Drugs.xlsx")

table_excel <- cbind( "inco1" = sum(excel_data$inco1 == 1), 
                      "inco2" = sum(excel_data$inco2 == 1), 
                      "inco3" = sum(excel_data$inco3 == 1), 
                      "inco4" = sum(excel_data$inco4 == 1), 
                      "inco5" = sum(excel_data$inco5 == 1), 
                      "inco6" = sum(excel_data$inco6 == 1), 
                      "inco7" = sum(excel_data$inco7 == 1))


#transform table to make it row-wise
table_excel <- t(table_excel)

#calculating total of each row i.e. inco1 to inco7
total_val <- sum(table_excel);

#row binding total_val to table
table_excel <- rbind(table_excel, "Total" = total_val)

#calculating percentage of each value
percent_table <- round(table_excel / total_val * 100, 1)
#column binding percentage
table_excel <- cbind(table_excel, percent_table)

#renaming columns
colnames(table_excel) <- c("Count", "Percent")

table_excel


# Get xtabs table for inco1 and crime1
inco1_crime1 <- xtabs(~ excel_data$inco1 + excel_data$crime1)
# Get row for crime1 = 1 and calculate total
inco1_crime1_percent <- prop.table(inco1_crime1)[2,]
# Calculate percentages
percent_crime1_1 <- sum(round(inco1_crime1_percent * 100, 1))


# Get xtabs table for inco2 and crime1
inco2_crime1 <- xtabs(~ excel_data$inco2 + excel_data$crime1)

# Get row for crime1 = 1 and calculate total
inco2_crime1_percent <- prop.table(inco2_crime1)[2,]
# Calculate percentages
inco2_crime1_percent
(0.48732084+ 0.04630650+ 0.07497244+ 0.02535832) * 100
percent_crime2_1 <- sum(round(inco2_crime1_percent * 100, 1))

# Get xtabs table for inco3 and crime1
inco3_crime1 <- xtabs(~ excel_data$inco3 + excel_data$crime1)
# Get row for crime1 = 1 and calculate total
inco3_crime1_percent <- prop.table(inco3_crime1)[2,]
# Calculate percentages
percent_crime3_1 <- sum(round(inco3_crime1_percent * 100, 1))

# Get xtabs table for inco4 and crime1
inco4_crime1 <- xtabs(~ excel_data$inco4 + excel_data$crime1)
# Get row for crime1 = 1 and calculate total
inco4_crime1_percent <- prop.table(inco4_crime1)[2,]
# Calculate percentages
percent_crime4_1 <- sum(round(inco4_crime1_percent * 100, 1))

# Get xtabs table for inco5 and crime1
inco5_crime1 <- xtabs(~ excel_data$inco5 + excel_data$crime1)
# Get row for crime1 = 1 and calculate total
inco5_crime1_percent <- prop.table(inco5_crime1)[2,]
# Calculate percentages
percent_crime5_1 <- sum(round(inco5_crime1_percent * 100, 1))

# Get xtabs table for inco6 and crime1
inco6_crime1 <- xtabs(~ excel_data$inco6 + excel_data$crime1)
# Get row for crime1 = 1 and calculate total
inco6_crime1_percent <- prop.table(inco6_crime1)[2,]
# Calculate percentages
percent_crime6_1 <- sum(round(inco6_crime1_percent * 100, 1))

# Get xtabs table for inco7 and crime1
inco7_crime1 <- xtabs(~ excel_data$inco7 + excel_data$crime1)
# Get row for crime1 = 1 and calculate total
inco7_crime1_percent <- prop.table(inco7_crime1)[2,]
# Calculate percentages
percent_crime7_1 <- sum(round(inco7_crime1_percent * 100, 1))

#creating new table for row binding in table_excel
table_percent <- rbind(percent_crime1_1, percent_crime2_1, percent_crime3_1, percent_crime4_1, percent_crime5_1, percent_crime6_1, percent_crime7_1)
table_percent <- rbind(table_percent, sum(table_percent))

#column binding in table_excel
table_excel <- cbind(table_excel, table_percent)
#renaming column name
colnames(table_excel) <- c("Count", "Percent", "Percent of Cases")
table_excel
