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
rides_wide$Date <- mdy(format(mdy_hms(rides_wide$Date), "%m/%d/%Y"))

# Divide columns by 1000
rides_wide$Bus  <- rides_wide$Bus  / 1000
rides_wide$Rail <- rides_wide$Rail / 1000

# Filter to date >= March 2020
rides_wide <- rides_wide %>%
  filter(Date >= mdy("04/01/2020"))

# Aggregate to monthly level
rides_monthly <- rides_wide %>%
  group_by(Date = floor_date(Date, "month")) %>%
  summarise(Rail_tot = sum(Rail),
            Bus_tot  = sum(Bus),
            Rail_avg = mean(Rail),
            Bus_avg  = mean(Bus))

# Change gas_prices to report day 01 instead of 15
gas_prices$Date <- floor_date(gas_prices$Date, "month")
gas_prices$Date <- as.Date(gas_prices$Date)
gas_prices <- gas_prices |> rename(gas_price = `Central Atlantic (PADD 1B) Regular All Formulations Retail Gasoline Prices (Dollars per Gallon)`)

# Merge gas prices with rides data
data <- merge(rides_monthly, gas_prices, by = "Date")
data$Date <- yearmonth(data$Date)

# Convert data to tsibble
data_ts <- as_tsibble(data, index = Date)

# line plot of Rail and gas price by date
ggplot(data_ts, aes(x = Date)) +
  geom_line(aes(y = Rail_avg, color = "Rail")) +
  geom_line(aes(y = gas_price, color = "Gas Price")) +
  scale_y_continuous(
    name = "Entries or Boardings",
    # Secondary axis transformation
    sec.axis = sec_axis(~./75, name = "Gas Price ($)")
  ) +
  labs(title = "Daily Ridership by Mode",
       x = "Date",
       y = "Entries or Boardings") +
  theme_minimal()


ggplot(data_ts, aes(x = Date)) +
  # Plot Rail_avg on primary scale
  geom_line(aes(y = Rail_avg, color = "Rail")) +
  # Scale up gas_price values for plotting on the primary scale
  geom_line(aes(y = gas_price * 100 - 200, color = "Gas Price")) +
  scale_y_continuous(
    name = "Average Daily Boardings (000s)",
    # Transform back to original gas price values for the secondary axis labels
    sec.axis = sec_axis(~ (.+ 200) / 100 , name = "Price ($/gal)")
  ) +
  labs(title = "Average Daily Boardings vs Gasoline Price",
       x = "") +
  theme_minimal() +
  theme(
    legend.position = "bottom"
  )
  
  
