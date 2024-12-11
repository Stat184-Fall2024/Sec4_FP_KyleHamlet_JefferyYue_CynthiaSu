##data scraping at it's finest
<<<<<<< Updated upstream
=======

# Load required libraries
library(rvest)
library(dplyr)
library(readr)


url <- "https://www.nba.com/stats/players/traditional?PerMode=Totals&Season=2000-01&dir=A&sort=TEAM_ABBREVIATION"


webpage <- read_html(url)


table <- webpage %>%
  html_table(fill = TRUE) %>%
  .[[1]] # Select the first table on the page



# Save the table to a CSV file
write_csv(table, "nba_player_stats.csv")

# Print a success message
cat("Data has been successfully saved to 'nba_player_stats.csv'\n")



# Read the CSV file
nba_data <- read.csv("nba_player_stats.csv")
>>>>>>> Stashed changes
