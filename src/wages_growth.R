# Packages

library(tidytable)
library(ggplot2)
library(ggthemes)
library(readabs)
library(janitor)

# Functions

read_and_clean_ABS <- function(cat_no, type = "Seasonally Adjusted") {
  
  readabs::read_abs(cat_no) |> 
    tidytable::filter(series_type == type) |> 
    tidytable::select(date, series, value) |>
    tidytable::pivot_wider(names_from = series,
                            values_from = value,
                            values_fn = tidytable::first) |> 
    janitor::clean_names()
}

custom_plot <- function(df) {
  
  df |> 
  pivot_longer(cols = -Date, names_to = "Measure", values_to = "Value") |> 
    ggplot(aes(x = Date, colour = Measure)) +
    geom_line(aes(y = Value)) +
    geom_hline(aes(yintercept = 0))
}

perc_diff <- function (x) c(diff(x)/(x[-length(x)]), NA)

# Data import and clean

key_aggregates <- read_and_clean_ABS(cat_no = "5206.0")

CPI <- read_and_clean_ABS(cat_no = "6401.0")

WPI <- read_and_clean_ABS(cat_no = "6345.0")

# Plotting

PCE_selected <- key_aggregates |>
  select(Date = date,
          NominalConsumption = final_consumption_expenditure_current_prices,
          RealConsumption = final_consumption_expenditure_chain_volume_measures) |>
  mutate(across(-Date, perc_diff)) |> 
  mutate(PCEInflation = ((1 + NominalConsumption)/(1 + RealConsumption) - 1)) |> 
  select(Date, PCEInflation)

CPI_selected <- CPI |> 
  select(Date = date,
          CPI = percentage_change_from_previous_period_all_groups_cpi_seasonally_adjusted_australia) |> 
  mutate(CPI = CPI/100)

inflation_selected <- WPI |> 
  select(Date = date,
          WageGrowth = percentage_change_from_previous_quarter_australia_total_hourly_rates_of_pay_excluding_bonuses_private_and_public_all_industries) |>
  mutate(WageGrowth = WageGrowth/100) |> 
  left_join(PCE_selected, by = 'Date') |>
  left_join(CPI_selected, by = 'Date') |> 
  na.omit()

inflation_selected |>
  filter(Date > "2015-01-01") |> 
  mutate(across(-Date, \(x) cumprod(1 + x))) |> 
  custom_plot()
     
  
