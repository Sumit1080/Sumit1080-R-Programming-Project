library(tidyverse)
library(TTR)

# ==========================================
# PRACTICAL: BOLLINGER BAND
# ==========================================

# Generate sample time-series data

set.seed(123)

sample_data <- data.frame(
  Day = 1:60,
  Price = cumsum(rnorm(60, mean = 0.5, sd = 2)) + 100
)

print(head(sample_data))


# Calculate Bollinger Bands

bands <- BBands(
  sample_data$Price,
  n = 10,
  sd = 2
)

# Combine results

sample_data <- bind_cols(
  sample_data,
  as.data.frame(bands)
)

print(head(sample_data, 15))



# ==========================================
# PRACTICAL: BOLLINGER BAND
# ==========================================

# Generate sample time-series data

set.seed(123)

sample_data <- data.frame(
  Day = 1:60,
  Price = cumsum(rnorm(60, mean = 0.5, sd = 2)) + 100
)

print(head(sample_data))



# Calculate Bollinger Bands

bands <- BBands(
  sample_data$Price,
  n = 10,
  sd = 2
)

# Combine results

sample_data <- bind_cols(
  sample_data,
  as.data.frame(bands)
)

print(head(sample_data, 15))



ggplot(sample_data, aes(x = Day)) +
  
  geom_ribbon(
    aes(ymin = dn, ymax = up),
    fill = "lightblue",
    alpha = 0.4
  ) +
  
  geom_line(
    aes(y = Price),
    color = "black",
    linewidth = 0.8
  ) +
  
  geom_line(
    aes(y = mavg),
    color = "blue",
    linewidth = 0.8
  ) +
  
  geom_line(
    aes(y = up),
    color = "red",
    linetype = "dashed"
  ) +
  
  geom_line(
    aes(y = dn),
    color = "darkgreen",
    linetype = "dashed"
  ) +
  
  labs(
    title = "Bollinger Band Visualization",
    subtitle = "10-period moving average with 2 standard deviations",
    x = "Day",
    y = "Sample Price"
  ) +
  
  theme_minimal()

ggsave(
  "visualizations/bollinger_band.png",
  width = 10,
  height = 6
)
