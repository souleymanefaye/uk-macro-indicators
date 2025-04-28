# ==============================================================================
# Macroeconometrics 
# Import the time series data from the UK statistical office
# ==============================================================================


# import GDP data
gdp_raw_data <- read_csv(
  paste0(
    "https://www.ons.gov.uk/generator?format=csv&uri=",
    "/economy/grossdomesticproductgdp/timeseries/abmi/ukea"
  ), 
  show_col_types = FALSE
)

# Remove annual data to keep quarterly
gdp_cleaned_data <- gdp_raw_data %>%
  filter(!row_number() %in% c(1:84))  %>%
  rename(
    date = Title,
    gdp = `Gross Domestic Product: chained volume measures: Seasonally adjusted £m`
  ) %>%
  mutate(
    # Convert the 'quarter' string into a yearquarter object
    date = yearquarter(date)
  ) %>%
  mutate(gdp = as.numeric(gdp)) # convert GDP to numeric


# import trade balance data 
trade_balance_raw_data <- read_csv(
  paste0(
    "https://www.ons.gov.uk/generator?format=csv&uri=/economy/",
    "nationalaccounts/balanceofpayments/timeseries/hbop/pnbp"
  ),
  show_col_types = FALSE
)

trade_balance_cleaned_data <- trade_balance_raw_data %>%
  filter(!row_number() %in% c(1:86))  %>%
  rename(
    date = Title,
    balance_payments =  `BoP Current Account Balance SA £m`
  ) %>%
  mutate(
    # Convert the 'quarter' string into a yearquarter object
    date = yearquarter(date)
  ) %>%
  mutate(balance_payments = as.numeric(balance_payments))
  
# import exchange rate data
exchange_rate_raw_data <- read_csv(
  paste0(
    "https://www.ons.gov.uk/generator?format=csv&uri=/economy/",
    "nationalaccounts/balanceofpayments/timeseries/thap/mret"
  ),
  show_col_types = FALSE
)

exchange_rate_cleaned_data <- exchange_rate_raw_data %>%
  filter(!row_number() %in% c(1:57))  %>%
  filter(!row_number() %in% c(201:802))  %>%
  rename(
    date = Title,
    exchange_rate =  `Average Sterling exchange rate: Euro XUMAERS`
  ) %>% mutate(
    # Convert the 'quarter' string into a yearquarter object
    date = yearquarter(date)
  ) %>%
  mutate(exchange_rate = as.numeric(exchange_rate))


uk_macro_indicators <- left_join(gdp_cleaned_data, trade_balance_cleaned_data, 
                                 by ="date") %>%
  left_join(exchange_rate_cleaned_data, by = "date")

write.csv(uk_macro_indicators, file = "work-data/data-uk.csv", row.names = FALSE)