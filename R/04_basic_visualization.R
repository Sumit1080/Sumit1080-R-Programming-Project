
library(tidyverse)
library(scales)

data_2021 <- read_csv(
  "data/processed/cleaned_2021_22.csv"
)

# Prepare enrolment data

enrolment_2021 <- data_2021 %>%
  select(
    State = State,
    Boys = `Enrolment of Age Group - All Ages - Pre-Primary to Higher Secondary - Boys`,
    Girls = `Enrolment of Age Group - All Ages - Pre-Primary to Higher Secondary - Girls`
  ) %>%
  filter(toupper(State) != "INDIA")


gender_distribution <- tibble(
  Gender = c("Boys", "Girls"),
  
  Enrolment = c(
    sum(enrolment_2021$Boys, na.rm = TRUE),
    sum(enrolment_2021$Girls, na.rm = TRUE)
  )
) %>%
  mutate(
    Percentage = Enrolment / sum(Enrolment) * 100
  )

print(gender_distribution)



# PIE CHART

ggplot(
  gender_distribution,
  aes(
    x = "",
    y = Enrolment,
    fill = Gender
  )
) +
  geom_col(width = 1) +
  coord_polar(theta = "y") +
  geom_text(
    aes(
      label = paste0(
        round(Percentage, 1), "%"
      )
    ),
    position = position_stack(vjust = 0.5)
  ) +
  labs(
    title = "Boys vs Girls School Enrolment",
    subtitle = "UDISE+ 2021-22",
    fill = "Gender"
  ) +
  theme_void()
ggsave(
  "visualizations/gender_pie_chart.png",
  width = 9,
  height = 6
)
