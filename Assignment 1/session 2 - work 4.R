library(readxl)

excel_data <- read_excel("D:/R programming runs/Assisgnments/Assignment 1/Manish Shah - MR_Drugs.xlsx")

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

total_row = nrow(excel_data)

percent_cases = round(table_excel[,1] / total_row * 100, 1)

#column binding in table_excel
table_excel <- cbind(table_excel, percent_cases)
#renaming column name
colnames(table_excel) <- c("N", "Percent", "Percent of Cases")
table_excel


