
# WORD CLOUD PRACTICAL

library(wordcloud)
library(RColorBrewer)
library(tidyverse)


# Education-related word frequencies

education_words <- data.frame(
  
  word = c(
    "Education",
    "Literacy",
    "Students",
    "Teachers",
    "Schools",
    "Enrolment",
    "Equality",
    "Learning",
    "Development",
    "Knowledge",
    "Girls",
    "Boys"
  ),
  
  frequency = c(
    100, 85, 75, 65,
    60, 55, 50, 45,
    40, 35, 30, 25
  )
)

print(education_words)



wordcloud(
  words = education_words$word,
  freq = education_words$frequency,
  
  min.freq = 1,
  max.words = 50,
  
  random.order = FALSE,
  rot.per = 0.15,
  
  scale = c(3.5, 0.7),
  
  colors = brewer.pal(8, "Dark2")
)

#save word cloud
png(
  "visualizations/education_wordcloud.png",
  width = 1600,
  height = 1100,
  res = 150
)

wordcloud(
  words = education_words$word,
  freq = education_words$frequency,
  min.freq = 1,
  random.order = FALSE,
  scale = c(3.5, 0.7),
  colors = brewer.pal(8, "Dark2")
)

dev.off()



# PRACTICAL: HEAT MAP

library(tidyverse)

literacy <- read_csv(
  "data/processed/cleaned_literacy_2011.csv"
)

# Prepare literacy data

literacy_analysis <- literacy %>%
  select(
    State,
    Total_Literacy = `Persons - All`,
    Male_Literacy = `Male - All`,
    Female_Literacy = `Female - All`
  ) %>%
  filter(toupper(State) != "INDIA") %>%
  mutate(
    Gender_Gap = Male_Literacy - Female_Literacy
  )


literacy_heatmap <- literacy_analysis %>%
  pivot_longer(
    cols = c(
      Total_Literacy,
      Male_Literacy,
      Female_Literacy,
      Gender_Gap
    ),
    names_to = "Indicator",
    values_to = "Value"
  )

print(head(literacy_heatmap))


ggplot(
  literacy_heatmap,
  aes(
    x = Indicator,
    y = State,
    fill = Value
  )
) +
  geom_tile(color = "white") +
  scale_fill_gradient(
    low = "lightyellow",
    high = "darkblue"
  ) +
  labs(
    title = "State-wise Literacy Heat Map",
    subtitle = "Census of India 2011",
    x = "Literacy Indicator",
    y = "State / UT",
    fill = "Percentage"
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 7)
  )

#Save the graph
ggsave(
  "visualizations/literacy_heatmap.png",
  width = 12,
  height = 9
)


# ==========================================
# PRACTICAL: DONUT CHART
# ==========================================

library(tidyverse)

# Prepare gender distribution

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

# Create donut chart

ggplot(
  gender_distribution,
  aes(
    x = 2,
    y = Enrolment,
    fill = Gender
  )
) +
  
  geom_col(width = 1) +
  
  coord_polar(theta = "y") +
  
  xlim(0.5, 2.5) +
  
  geom_text(
    aes(
      label = paste0(
        round(Percentage, 1),
        "%"
      )
    ),
    position = position_stack(vjust = 0.5)
  ) +
  
  labs(
    title = "Gender-wise School Enrolment",
    subtitle = "UDISE+ 2021-22",
    fill = "Gender"
  ) +
  
  theme_void()
ggsave(
  "visualizations/gender_donut_chart.png",
  width = 9,
  height = 6
)


library(tidyverse)
library(gganimate)
library(gifski)
library(tweenr)
# ==========================================
# DYNAMIC ENROLMENT DATA
# ==========================================

# Prepare 2020-21 data

dynamic_2020 <- data_2020 %>%
  select(
    State,
    Enrolment =
      `Enrolment of Age Group - All Ages - Pre-Primary to Higher Secondary - Total`
  ) %>%
  filter(toupper(State) != "INDIA") %>%
  mutate(Year = "2020-21")


# Prepare 2021-22 data

dynamic_2021 <- data_2021 %>%
  select(
    State,
    Enrolment =
      `Enrolment of Age Group - All Ages - Pre-Primary to Higher Secondary - Total`
  ) %>%
  filter(toupper(State) != "INDIA") %>%
  mutate(Year = "2021-22")


# Combine both datasets

dynamic_data <- bind_rows(
  dynamic_2020,
  dynamic_2021
)

# Check output

print(head(dynamic_data))

dim(dynamic_data)

top_states <- dynamic_data %>%
  group_by(State) %>%
  summarise(
    Average_Enrolment = mean(Enrolment),
    .groups = "drop"
  ) %>%
  arrange(desc(Average_Enrolment)) %>%
  slice_head(n = 10) %>%
  pull(State)

dynamic_data <- dynamic_data %>%
  filter(State %in% top_states)

dim(dynamic_data)
head(dynamic_data)


# ==========================================
# DYNAMIC SCHOOL ENROLMENT VISUALIZATION
# ==========================================

dynamic_chart <- ggplot(
  dynamic_data,
  aes(
    x = reorder(State, Enrolment),
    y = Enrolment,
    fill = Year
  )
) +
  
  geom_col(
    width = 0.7
  ) +
  
  coord_flip() +
  
  scale_y_continuous(
    labels = scales::comma
  ) +
  
  labs(
    title = "School Enrolment Comparison",
    subtitle = "Academic Year: {closest_state}",
    x = "State / UT",
    y = "Total Enrolment",
    fill = "Academic Year"
  ) +
  
  theme_minimal() +
  
  transition_states(
    Year,
    transition_length = 2,
    state_length = 1
  ) +
  
  ease_aes("cubic-in-out")

animated_chart <- animate(
  dynamic_chart,
  nframes = 60,
  fps = 10,
  width = 1000,
  height = 700,
  renderer = gifski_renderer()
)

anim_save(
  "visualizations/enrolment_animation.gif",
  animation = animated_chart
)
