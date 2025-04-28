# ==============================================================================
# Macroeconometrics 
# Analysis
# ==============================================================================

# Clear environment
rm(list=ls())

#
#install.packages("patchwork")  
library(patchwork)

# 
uk_data <- read_csv("work-data/data-uk.csv", show_col_types = FALSE)
uk_data$date <- as.Date(as.yearqtr(uk_data$date, format = "%Y Q%q"))
uk_data$date <- as.yearqtr(uk_data$date, format = "Q%q %Y")

ggplot(uk_data, aes(x = date, y = gdp)) +
  geom_line(color = "#2c7bb6", linewidth = 0.8) +
  scale_x_yearqtr(
    name = "Year",
    format = "Q%q %Y",
    breaks = seq(min(uk_data$date), max(uk_data$date), by = 3), # Every 2 years
    expand = c(0, 0)
  ) +
  scale_y_continuous(
    name = "GDP (in millions of GBP)",
    labels = scales::comma_format(scale = 1e-6), # Convert to millions, add commas
    expand = c(0, 0)
  ) +
  labs(
    title = "UK GDP Over Time (1960–2023)",
    caption = "Source: UK Statistical Office."
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold", margin = margin(b = 10)),
    axis.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 10),
    axis.text.x = element_text(angle = 45, hjust = 1), # Rotate x-axis labels for readability
    plot.caption = element_text(size = 8, hjust = 0, margin = margin(t = 10)),
    panel.grid.major = element_line(color = "grey90", linewidth = 0.5),
    panel.grid.minor = element_blank(),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA)
  )

#
ggplot(uk_data, aes(x = date, y = balance_payments)) +
  geom_line(color = "#2c7bb6", linewidth = 0.8) +
  scale_x_yearqtr(
    name = "Year",
    format = "Q%q %Y",
    breaks = seq(min(uk_data$date), max(uk_data$date), by = 3), # Every 2 years
    expand = c(0, 0)
  ) +
  scale_y_continuous(
    name = "Balance of Payments (in millions of GBP)"
  ) +
  labs(
    title = "UK Balance of Payments Over Time (1960–2024)",
    caption = "Source: UK Statistical Office."
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold", margin = margin(b = 10)),
    axis.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 10),
    axis.text.x = element_text(angle = 45, hjust = 1), # Rotate x-axis labels for readability
    plot.caption = element_text(size = 8, hjust = 0, margin = margin(t = 10)),
    panel.grid.major = element_line(color = "grey90", linewidth = 0.5),
    panel.grid.minor = element_blank(),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA)
  )

#
ggplot(uk_data, aes(x = date, y = exchange_rate)) +
  geom_line(color = "#2c7bb6", linewidth = 0.8) +
  scale_x_yearqtr(
    name = "Year",
    format = "Q%q %Y",
    breaks = seq(min(uk_data$date), max(uk_data$date), by = 3), 
    expand = c(0, 0)
  ) +
  scale_y_continuous(
    name = "Average Sterling exchange rate"
  ) +
  labs(
    title = "UK Exchange Rate Over Time (1997–2024)",
    caption = "Source: UK Statistical Office."
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold", margin = margin(b = 10)),
    axis.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 10),
    axis.text.x = element_text(angle = 45, hjust = 1), # Rotate x-axis labels for readability
    plot.caption = element_text(size = 8, hjust = 0, margin = margin(t = 10)),
    panel.grid.major = element_line(color = "grey90", linewidth = 0.5),
    panel.grid.minor = element_blank(),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA)
  )


# unit root and stationarity tests 