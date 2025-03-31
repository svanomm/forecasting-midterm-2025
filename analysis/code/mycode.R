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
data <- as_tsibble(data, index = Date)


# Seasonality graph
data |> gg_subseries(y=Rail_avg) +
  labs(title = "Average Daily Rail Boardings",
       y="Average Daily Boardings (000s)",
       subtitle = "Seasonal Subseries Plot",
       x="",
       caption = "Source: Washington Metropolitan Area Transit Authority, Daily Ridership Dashboards.")
ggsave(here("./analysis/output/graphs/Seasonality Plot.png"), width = 10, height = 6)

# Split the data 
train <- data |> slice(1:48)
test  <- data |> slice(49:60)

# Create variable train_test which is "train" for observations 1:48 and "test" else
data <- data |> mutate(
  train_test = if_else(row_number() <= 48, "train", "test")
  )

# Plot Rail_avg over time, colored by train_test
ggplot(data, aes(x = Date)) +
  # Plot Rail_avg on primary scale
  geom_line(aes(y = Rail_avg, color = train_test)) +
  labs(title = "Average Daily Rail Boardings",
       y = "Average Daily Boardings (000s)",
       x = "",
       caption = "Sources: Washington Metropolitan Area Transit Authority, Daily Ridership Dashboards.") +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    panel.border = element_rect(colour = "black", fill=NA, linewidth=1)
  )
ggsave(here("./analysis/output/graphs/train_test.png"), width = 10, height = 6)


# Plot Rail vs gas price
ggplot(data, aes(x = Date)) +
  # Plot Rail_avg on primary scale
  geom_line(aes(y = Rail_avg, color = "Rail")) +
  # Scale up gas_price values for plotting on the primary scale
  geom_line(aes(y = gas_price * 100 - 200, color = "Gas Price")) +
  scale_y_continuous(
    name = "Average Daily Boardings (000s)",
    # Transform back to original gas price values for the secondary axis labels
    sec.axis = sec_axis(~ (.+ 200) / 100 , name = "Price ($/gal)")
  ) +
  labs(title = "Average Daily Rail Boardings vs Gasoline Price",
       x = "",
       caption = "Sources: Washington Metropolitan Area Transit Authority, Daily Ridership Dashboards,\n U.S. Energy Information Administration, Petroleum & Other Liquids.") +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    panel.border = element_rect(colour = "black", fill=NA, linewidth=1)
  )
ggsave(here("./analysis/output/graphs/rides_vs_gas.png"), width = 10, height = 6)


STL_decomp <- data |> 
  model(stl = STL(Rail_avg))

STL_decomp |> components() |>
  autoplot() +
  labs(title = "STL decomposition: Average Daily Rail Boardings",
       y="Average Daily Boardings (000s)",
       x="",
       caption = "Source: Washington Metropolitan Area Transit Authority, Daily Ridership Dashboards.")
ggsave(here("./analysis/output/graphs/STL_Decomp.png"), width = 10, height = 6)

# For forecasting, we need future values of gas prices.
# Use ARIMA to forecast gas prices
gas_model <- train |>
  model(ARIMA(gas_price ~ pdq(1, 1, 1) + PDQ(1, 1, 1))) |>
  forecast(new_data = test)
# Update the testing data with the predicted gas prices
test <- test |> mutate(
  gas_price = gas_model$.mean
)

knot1 <- yearmonth("2021 Jan")
knot2 <- yearmonth("2022 Jan")
knot3 <- yearmonth("2023 Jan")
knot4 <- yearmonth("2024 Jan")

# Fit models
my_models <- train |>
  model(
    snaive = SNAIVE(Rail_avg),
    ets    = ETS(Rail_avg ~ trend("Ad") + season("A")),
    reg_control = TSLM(Rail_avg ~ gas_price + season() + trend(knots = c(knot1,knot2,knot3,knot4))),
    arimax = ARIMA(Rail_avg ~ gas_price)
    ) |>
  mutate(ensemble = (snaive+ets+reg_control+arimax)/4)

my_forecasts <- my_models |>
  forecast(new_data = test)

my_models |> select(snaive) |> gg_tsresiduals() + 
  labs(title = "Residuals of Time Series Linear Model", 
       x="")
#ggsave(here("Model 1 Residuals.png"), width = 10, height = 6)

my_models |> select(arima_simple) |> gg_tsresiduals() + 
  labs(title = "Residuals of ARIMA Model", 
       x="")
#ggsave(here("Model 2 Residuals.png"), width = 10, height = 6)

my_models |> select(arima_control) |> gg_tsresiduals()

my_models |> select(reg_control) |> report()
my_models |> select(arima_simple) |> tidy()
my_models |> select(arima_control) |> tidy()



# Plot the model predictions on the training data
my_models |> fitted() |> autoplot()  + 
  theme(legend.position="bottom") +
  facet_grid(vars(.model), scales = "free_y") 


# Plot the model predictions on the testing data
my_forecasts |>
  autoplot(test, level = NULL) +
  theme(legend.position = "bottom") +
  labs(
    y = "Thousands",
    title = "Forecasts of Australian Thefts",
    x="",
    caption = "Notes: Models are trained on data from Jan. 1995 to Dec. 2014.\nSource: fpp3 'nsw_offences' data, Australian Bureau of Statistics."
  ) +
  guides(colour = guide_legend(title = "Forecast"))
ggsave(here("./analysis/output/graphs/forecasts.png"), width = 10, height = 6)


# IS and OOS accuracy metrics
is_accuracy  <- my_models |> accuracy()
oos_accuracy <- accuracy(my_forecasts, test)
combined_accuracy <- rbind(is_accuracy, oos_accuracy) |> arrange(desc(.type), RMSE)

d <- as.matrix(mutate_if(
  combined_accuracy, is.numeric, ~round(., 2)
))
stargazer(d, out = here("./analysis/output/graphs/Accuracy Table.txt"), type = "text")
