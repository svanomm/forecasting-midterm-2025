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
data <- left_join(rides_monthly, gas_prices, by = "Date")
data$Date <- yearmonth(data$Date)

# add March gas price data using https://www.eia.gov/dnav/pet/pet_pri_gnd_dcus_r1y_w.htm
# March weekly prices: 3.118	3.071	3.054	3.075. Average: 3.0795
data <- data |> mutate(
  gas_price = ifelse(Date == yearmonth("2025 Mar"), 3.0795, gas_price)
)

# Convert data to tsibble
data <- as_tsibble(data, index = Date)

# Seasonality graph
data |> gg_subseries(y=Rail_avg) +
  labs(title = "Average Daily Rail Boardings",
       y="Average Daily Boardings (000s)",
       subtitle = "Seasonal Subseries Plot",
       x="",
       caption = "Source: Washington Metropolitan Area Transit Authority, Daily Ridership Dashboards.")
ggsave(here("./analysis/output/graphs/Seasonality Plot.png"), width = 8, height = 5)

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
  ) +
  guides(colour = guide_legend(title = ""))
ggsave(here("./analysis/output/graphs/train_test.png"), width = 8, height = 5)


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
ggsave(here("./analysis/output/graphs/rides_vs_gas.png"), width = 8, height = 5)


STL_decomp <- data |> 
  model(stl = STL(Rail_avg))

STL_decomp |> components() |>
  autoplot() +
  labs(title = "STL decomposition: Average Daily Rail Boardings",
       y="Average Daily Boardings (000s)",
       x="",
       caption = "Source: Washington Metropolitan Area Transit Authority, Daily Ridership Dashboards.")
ggsave(here("./analysis/output/graphs/STL_Decomp.png"), width = 8, height = 5)

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

# Create a dataset with the residuals for my models
r_snaive      <- my_models |> select(snaive     ) |> residuals() |> select(Date, .resid) |> rename(snaive      = .resid)
r_ets         <- my_models |> select(ets        ) |> residuals() |> select(Date, .resid) |> rename(ets         = .resid)
r_reg_control <- my_models |> select(reg_control) |> residuals() |> select(Date, .resid) |> rename(reg_control = .resid)
r_arimax      <- my_models |> select(arimax     ) |> residuals() |> select(Date, .resid) |> rename(arimax      = .resid)
r_ensemble    <- my_models |> select(ensemble   ) |> residuals() |> select(Date, .resid) |> rename(ensemble    = .resid)

r_train <- r_snaive |> 
  inner_join(r_ets, by = "Date") |>
  inner_join(r_reg_control, by = "Date") |>
  inner_join(r_arimax, by = "Date") |>
  inner_join(r_ensemble, by = "Date")

r_snaive      <- as.data.frame(my_forecasts) |> filter(.model == "snaive"     ) |> select(Date, .mean) |> rename(snaive = .mean)
r_ets         <- as.data.frame(my_forecasts) |> filter(.model == "ets"        ) |> select(Date, .mean) |> rename(ets = .mean)
r_reg_control <- as.data.frame(my_forecasts) |> filter(.model == "reg_control") |> select(Date, .mean) |> rename(reg_control = .mean)
r_arimax      <- as.data.frame(my_forecasts) |> filter(.model == "arimax"     ) |> select(Date, .mean) |> rename(arimax = .mean)
r_ensemble    <- as.data.frame(my_forecasts) |> filter(.model == "ensemble"   ) |> select(Date, .mean) |> rename(ensemble = .mean)

r_test <- test  |> 
  inner_join(r_snaive, by = "Date") |>
  inner_join(r_ets, by = "Date") |>
  inner_join(r_reg_control, by = "Date") |>
  inner_join(r_arimax, by = "Date") |>
  inner_join(r_ensemble, by = "Date")

r_test <- r_test |> mutate(
  snaive = Rail_avg - snaive,
  ets = Rail_avg - ets,
  reg_control = Rail_avg - reg_control,
  arimax = Rail_avg - arimax,
  ensemble = Rail_avg - ensemble
) |> select(Date, Rail_avg, snaive, ets, reg_control, arimax, ensemble)

# append r_train and r_test
residuals <- r_train |> bind_rows(r_test)

# Plot all the residuals over time
ggplot(residuals, aes(x = Date)) +
  geom_line(aes(y = ets, color = "ets")) +
  geom_line(aes(y = reg_control, color = "reg_control")) +
  geom_line(aes(y = arimax, color = "arimax")) +
  geom_line(aes(y = ensemble, color = "ensemble")) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  labs(title = "Residuals of Models",
       y = "Residuals",
       x = "",
       caption = "Sources: Washington Metropolitan Area Transit Authority, Daily Ridership Dashboards,\n U.S. Energy Information Administration, Petroleum & Other Liquids.") +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    panel.border = element_rect(colour = "black", fill=NA, linewidth=1)
  ) +
  guides(colour = guide_legend(title = ""))
ggsave(here("./analysis/output/graphs/residuals.png"), width = 8, height = 5)
  

my_models |> select(ensemble) |> gg_tsresiduals() + 
  labs(title = "Residuals of Ensemble Model", 
       x="")

# Plot the model predictions on the testing data
my_forecasts |>
  autoplot(test, level = NULL) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    panel.border = element_rect(colour = "black", fill=NA, linewidth=1)
  ) + 
  labs(
    y = "Average Daily Boardings (000s)",
    title = "Forecasts of Average Daily Rail Boardings",
    x="",
    caption = "Sources: Washington Metropolitan Area Transit Authority, Daily Ridership Dashboards,\n U.S. Energy Information Administration, Petroleum & Other Liquids.") +
  guides(colour = guide_legend(title = ""))
ggsave(here("./analysis/output/graphs/forecasts.png"), width = 8, height = 5)


# IS and OOS accuracy metrics
is_accuracy  <- my_models |> accuracy()
oos_accuracy <- accuracy(my_forecasts, test)
combined_accuracy <- rbind(is_accuracy, oos_accuracy) |> arrange(desc(.type), RMSE)

d <- as.matrix(mutate_if(
  combined_accuracy, is.numeric, ~round(., 2)
))
stargazer(d, out = here("./analysis/output/graphs/Accuracy Table.tex"), type = "latex")

stargazer(d, type = "text")
