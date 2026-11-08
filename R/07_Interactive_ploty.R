library(tidyverse)
library(plotly)
library(scales)

# ==========================================
# PRACTICAL 10: INTERACTIVE PLOTLY
# ==========================================

data_2021 <- read_csv(
  "data/processed/cleaned_2021_22.csv"
)

# Prepare gender-wise enrolment data

enrolment_2021 <- data_2021 %>%
  select(
    State,
    Boys = `Enrolment of Age Group - All Ages - Pre-Primary to Higher Secondary - Boys`,
    Girls = `Enrolment of Age Group - All Ages - Pre-Primary to Higher Secondary - Girls`
  ) %>%
  filter(toupper(State) != "INDIA")

print(head(enrolment_2021))



# Convert into long format

gender_plotly <- enrolment_2021 %>%
  pivot_longer(
    cols = c(Boys, Girls),
    names_to = "Gender",
    values_to = "Enrolment"
  )

# Create interactive chart

p1 <- ggplot(
  gender_plotly,
  aes(
    x = State,
    y = Enrolment,
    fill = Gender,
    text = paste(
      "State:", State,
      "<br>Gender:", Gender,
      "<br>Enrolment:", comma(Enrolment)
    )
  )
) +
  
  geom_col(position = "dodge") +
  
  labs(
    title = "Interactive Gender-wise School Enrolment",
    x = "State / UT",
    y = "Total Enrolment"
  ) +
  
  theme_minimal() +
  
  theme(
    axis.text.x = element_text(
      angle = 90,
      hjust = 1,
      size = 7
    )
  )

# Convert ggplot into Plotly

interactive_bar <- ggplotly(
  p1,
  tooltip = "text"
)

interactive_bar
# Save interactive bar chart

htmlwidgets::saveWidget(
  interactive_bar,
  "visualizations/interactive_enrolment.html"
)

# Convert enrolment data into long format

gender_plotly <- enrolment_2021 %>%
  pivot_longer(
    cols = c(Boys, Girls),
    names_to = "Gender",
    values_to = "Enrolment"
  )

# Check the result

print(head(gender_plotly))
dim(gender_plotly)


p1 <- ggplot(
  gender_plotly,
  aes(
    x = State,
    y = Enrolment,
    fill = Gender,
    text = paste(
      "State:", State,
      "<br>Gender:", Gender,
      "<br>Enrolment:", scales::comma(Enrolment)
    )
  )
) +
  geom_col(position = "dodge") +
  labs(
    title = "Interactive Gender-wise School Enrolment",
    x = "State / UT",
    y = "Total Enrolment"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(
      angle = 90,
      hjust = 1,
      size = 7
    )
  )

print(p1)


interactive_bar <- ggplotly(
  p1,
  tooltip = "text"
)

interactive_bar

htmlwidgets::saveWidget(
  interactive_bar,
  "visualizations/interactive_enrolment.html"
)




# INTERACTIVE SCATTER PLOT
# MALE VS FEMALE LITERACY


# Separate state data and India data
state_data <- literacy_comparison %>%
  filter(toupper(trimws(State)) != "INDIA")

india_data <- literacy_comparison %>%
  filter(toupper(trimws(State)) == "INDIA")

# Create improved interactive graph
interactive_scatter <- plot_ly() %>%
  
  # State points
  add_trace(
    data = state_data,
    x = ~Male,
    y = ~Female,
    type = "scatter",
    mode = "markers",
    name = "Male literacy higher",
    marker = list(
      color = "#f0c419",
      size = 10
    ),
    text = ~State,
    hovertemplate = paste(
      "<b>%{text}</b>",
      "<br>Male: %{x}%",
      "<br>Female: %{y}%",
      "<extra></extra>"
    )
  ) %>%
  
  # Equality line
  add_trace(
    x = c(0, 100),
    y = c(0, 100),
    type = "scatter",
    mode = "lines",
    name = "Equal literacy",
    line = list(color = "gray", width = 2),
    hoverinfo = "skip"
  ) %>%
  
  # Highlight India
  add_trace(
    data = india_data,
    x = ~Male,
    y = ~Female,
    type = "scatter",
    mode = "markers",
    name = "India",
    marker = list(
      color = "#2ca25f",
      size = 18,
      symbol = "circle",
      line = list(color = "black", width = 1)
    ),
    text = ~State,
    hovertemplate = paste(
      "<b>%{text}</b>",
      "<br>Male: %{x}%",
      "<br>Female: %{y}%",
      "<extra></extra>"
    )
  ) %>%
  
  layout(
    title = "Comparing Male and Female Literacy Rates Across Indian States",
    xaxis = list(
      title = "Male Literacy Rate (%)",
      range = c(0, 100)
    ),
    yaxis = list(
      title = "Female Literacy Rate (%)",
      range = c(0, 100),
      scaleanchor = "x",
      scaleratio = 1
    )
  )

# Display
interactive_scatter

# Save
htmlwidgets::saveWidget(
  interactive_scatter,
  "visualizations/interactive_literacy.html"
)



