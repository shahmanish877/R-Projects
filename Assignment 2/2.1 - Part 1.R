library(jsonlite)

#1. Get this data in R, make it tidy and do the necessary transformations: https://www.mohfw.gov.in/"

# Get json data
covid_data <- jsonlite::fromJSON("https://www.mohfw.gov.in/data/datanew.json")

# Convert json data to a data frame
covid_df <- as.data.frame(covid_data)

# Remove unnecessary columns
covid_df <- covid_df[, c("state_name", "new_active", "new_positive", "new_cured", "new_death")]

# Rename columns
colnames(covid_df) <- c("state", "active", "confirmed", "recovered", "deaths")

# convert data types from character to numeric
covid_df$active <- as.numeric(covid_df$active)
covid_df$confirmed <- as.numeric(covid_df$confirmed)
covid_df$recovered <- as.numeric(covid_df$recovered)
covid_df$deaths <- as.numeric(covid_df$deaths)


# Adding text to last row as "Total"
covid_df[nrow(covid_df), "state"] <- "Total"

# Print data frame
covid_df
str(covid_df)



#2. Get this data in R, make it tidy and do the necessary transformation: https://covid19.who.int/table


library(rvest)
library(tidyverse)
library(xml2)


# Read the HTML code of the page
page <- read_html("https://covid19.who.int/table")

# Extract the div element containing the role="table" data-attribute 
table_div <- page %>% html_nodes("div[role='table']")


# Extract the column names from the table
# First, remove svg icons from thead
remove_svg <- page %>% html_nodes("div.thead div.th svg")
xml_remove(remove_svg)

column_names <- table_div %>% html_nodes("div.thead div.th") %>% html_text()
column_names


# Extract data i.e. table row (tr)

table_rows <- page %>% html_nodes(".tr.depth_0")
table_rows <- table_rows[-c(1,2)]

# Extract the child nodes and their text values from each row
rows_data <- lapply(table_rows, function(row) {
  child_nodes <- row %>% html_children()
  child_values <- sapply(child_nodes, html_text)
  return(list(child_values))
})


# Convert the extracted data into a data frame
table_df <- as.data.frame(rows_data)


# Transpose the table_df & reset the row names to index and change the colnames to column_names extracted before
table_df <- t(table_df)
rownames(table_df) <- NULL
colnames(table_df) <- column_names

# Remove unnecessary columns
table_df <- table_df[, -c(3,5)]


#3. Why the original version of the imbd dataset by Kaggle was taken down? https://www.kaggle.com/datasets/tmdb/tmdb-movie-metadata
# As mentioned in "Data Source Transfer Summary", Kaggle had used dataset from IMDB without IMDB permission due to which IMDB filed a takedown notice as per DMCA act.


#4. Comment on this website's web scrapping policy: https://help.imdb.com/article/imdb/general-information/can-i-use-imdb-data-in-my-software/G5JTRESSHJBBHTGX#
# IMDB allows the data from https://contribute.imdb.com/dataset to be used for personal or educational purpose but doesn't allow to use web scraping on their website. For commercial use, we need to get an API which may cost some amount also.

