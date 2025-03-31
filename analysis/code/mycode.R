library(fpp3)
library(here)
library(stargazer)
library(dplyr)
library(readr)
library(lubridate)

here()

rides     <- read_csv(here("./data/Daily Ridership - Data View_data.csv"))
gas_prices <- read_csv(here("./data/EMM_EPMR_PTE_R1Y_DPGm.xls"))

# Read each CSV file into a list of data frames
data_list <- lapply(data_files, read_csv)

# Define a function to clean each data frame
clean_data <- function(df) {
  df %>%
    mutate(
      # Convert date column to Date type (assuming the column is named 'date')
      date = as.Date(date, format = "%Y-%m-%d"),
      # Handle missing values (e.g., replace with NA)
      across(everything(), ~ ifelse(. == "", NA, .))
    )
}

# Clean each data frame in the list
cleaned_data_list <- lapply(data_list, clean_data)
