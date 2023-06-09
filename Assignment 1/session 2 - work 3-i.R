library(haven)

sav_data <- read_sav("D:/R programming runs/Manish Shah - SAQ8.sav")
data_label <- attr(sav_data$q01, "label")
data_values <- sav_data$q01

#frequency table
freq_table <- table(data_values)

#percentage of frequency
percent_table <- round(freq_table / length(data_values) * 100, 1)

#valid percentage of frequency
valid_percent <- round(freq_table / sum(!is.na(data_values)) * 100, 1)

#cumulative percentage
cumul_percent <- cumsum(percent_table)

# Combining frequency and percentage into a single table
table_df <- cbind(freq_table, percent_table, valid_percent, cumul_percent)

# Add row names to the table
rownames(table_df) <- c("Strongly agree", "Agree", "Neither", "Disagree", "Strongly disagree")

# Rename the columns of the table
colnames(table_df) <- c("Frequency", "Percent", "Valid Percent", "Cumulative Percent")

# Calculate the total frequency and percentage
total_freq <- sum(table_df[ , 1])
total_percent <- sum(table_df[ , 2])
total_valid <- sum(table_df[ , 3])

# Create a new row with the total frequency and percentage
total_row <- c(total_freq, total_percent, total_valid, NA)

# Append the total row to the table
table_df <- rbind(table_df, total_row)

# Add row name for the total row
rownames(table_df)[nrow(table_df)] <- "Total"

cat("Final table of '", data_label, "'\n", sep='')
table_df