library(fpp3)
library(here)
library(stargazer)
library(dplyr)
library(readr)
library(lubridate)
library(readxl)

here()

rides      <- read_csv(here("./data/Daily Ridership - Data View_data.csv"))
gas_prices <- read_excel(here("./data/EMM_EPMR_PTE_R1Y_DPGm.xls"), sheet = "Data 1", range = "A3:B386")

# reshape rides data wide by Mode
rides_wide <- rides %>%
  pivot_wider(names_from = Mode, values_from = "Entries Or Boardings")

# Convert the datetime column to a date
rides_wide$Date <- as_date(rides_wide$Date)
