
# ==========================================
# EDUCATION EQUALITY IN INDIA
# PHASE 4: EXPLORATORY DATA ANALYSIS
# ==========================================

library(tidyverse)
library(scales)

# Import cleaned datasets

data_2020 <- read_csv(
  "data/processed/cleaned_2020_21.csv"
)

data_2021 <- read_csv(
  "data/processed/cleaned_2021_22.csv"
)

data_2023 <- read_csv(
  "data/processed/cleaned_2023_24.csv"
)

literacy <- read_csv(
  "data/processed/cleaned_literacy_2011.csv"
)

# Check dimensions

dim(data_2020)
dim(data_2021)
dim(data_2023)
dim(literacy)

# Display first few records

head(literacy)
head(data_2023)


# ==========================================
# ANALYSIS 1: STATE-WISE LITERACY ANALYSIS
# ==========================================

# Select important columns

literacy_analysis <- literacy %>%
  select(
    State,
    Total_Literacy = `Persons - All`,
    Male_Literacy = `Male - All`,
    Female_Literacy = `Female - All`
  )

# Remove national aggregate for state ranking

literacy_analysis <- literacy_analysis %>%
  filter(toupper(State) != "INDIA")

# Calculate gender literacy gap

literacy_analysis <- literacy_analysis %>%
  mutate(
    Gender_Gap = Male_Literacy - Female_Literacy
  )

# Display data

print(literacy_analysis)


# Top 10 states by literacy rate

top_10_literacy <- literacy_analysis %>%
  arrange(desc(Total_Literacy)) %>%
  slice_head(n = 10)

print(top_10_literacy)



# Bottom 10 states by literacy rate

bottom_10_literacy <- literacy_analysis %>%
  arrange(Total_Literacy) %>%
  slice_head(n = 10)

print(bottom_10_literacy)

# Average literacy rates across the
# state and UT observations in this dataset

literacy_summary <- literacy_analysis %>%
  summarise(
    Average_Total = mean(Total_Literacy),
    Average_Male = mean(Male_Literacy),
    Average_Female = mean(Female_Literacy),
    Average_Gender_Gap = mean(Gender_Gap)
  )

print(literacy_summary)



# ==========================================
# VISUALIZATION 1:
# TOP 10 STATES BY LITERACY RATE
# ==========================================

ggplot(
  top_10_literacy,
  aes(
    x = reorder(State, Total_Literacy),
    y = Total_Literacy
  )
) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(
    title = "Top 10 States by Literacy Rate",
    subtitle = "Census of India 2011",
    x = "State / UT",
    y = "Literacy Rate (%)"
  ) +
  theme_minimal()


# Convert data into long format

gender_literacy <- literacy_analysis %>%
  select(State, Male_Literacy, Female_Literacy) %>%
  pivot_longer(
    cols = c(Male_Literacy, Female_Literacy),
    names_to = "Gender",
    values_to = "Literacy_Rate"
  )

# Create comparison chart

ggplot(
  gender_literacy,
  aes(
    x = reorder(State, Literacy_Rate),
    y = Literacy_Rate,
    fill = Gender
  )
) +
  geom_col(position = "dodge") +
  coord_flip() +
  labs(
    title = "Male vs Female Literacy Rates",
    subtitle = "Census of India 2011",
    x = "State / UT",
    y = "Literacy Rate (%)",
    fill = "Gender"
  ) +
  theme_minimal()


# ==========================================
# ANALYSIS 4: GENDER-WISE SCHOOL ENROLMENT
# ==========================================

# Select important columns from 2020-21

enrolment_2020 <- data_2020 %>%
  select(
    State,
    Boys = `Enrolment of Age Group - All Ages - Pre-Primary to Higher Secondary - Boys`,
    Girls = `Enrolment of Age Group - All Ages - Pre-Primary to Higher Secondary - Girls`,
    Total = `Enrolment of Age Group - All Ages - Pre-Primary to Higher Secondary - Total`
  ) %>%
  filter(toupper(State) != "INDIA")

# Select important columns from 2021-22

enrolment_2021 <- data_2021 %>%
  select(
    State,
    Boys = `Enrolment of Age Group - All Ages - Pre-Primary to Higher Secondary - Boys`,
    Girls = `Enrolment of Age Group - All Ages - Pre-Primary to Higher Secondary - Girls`,
    Total = `Enrolment of Age Group - All Ages - Pre-Primary to Higher Secondary - Total`
  ) %>%
  filter(toupper(State) != "INDIA")

# Calculate gender indicators

enrolment_2020 <- enrolment_2020 %>%
  mutate(
    Gender_Gap = Boys - Girls,
    Girls_Percentage = (Girls / Total) * 100,
    Boys_Percentage = (Boys / Total) * 100
  )

enrolment_2021 <- enrolment_2021 %>%
  mutate(
    Gender_Gap = Boys - Girls,
    Girls_Percentage = (Girls / Total) * 100,
    Boys_Percentage = (Boys / Total) * 100
  )

# Display results

print(head(enrolment_2020))
print(head(enrolment_2021))



# Top 10 states by girls' enrolment

top_10_girls <- enrolment_2021 %>%
  arrange(desc(Girls)) %>%
  slice_head(n = 10)

print(top_10_girls)


# States with the largest boys-girls enrolment gap

gender_gap_analysis <- enrolment_2021 %>%
  arrange(desc(Gender_Gap))

print(head(gender_gap_analysis, 10))



# Boys vs Girls enrolment comparison

gender_enrolment_long <- enrolment_2021 %>%
  select(State, Boys, Girls) %>%
  pivot_longer(
    cols = c(Boys, Girls),
    names_to = "Gender",
    values_to = "Enrolment"
  )

ggplot(
  gender_enrolment_long,
  aes(
    x = reorder(State, Enrolment),
    y = Enrolment,
    fill = Gender
  )
) +
  geom_col(position = "dodge") +
  coord_flip() +
  labs(
    title = "Boys vs Girls School Enrolment",
    subtitle = "UDISE+ 2021-22",
    x = "State / UT",
    y = "Number of Students",
    fill = "Gender"
  ) +
  scale_y_continuous(labels = comma) +
  theme_minimal()


ggplot(
  enrolment_2021,
  aes(
    x = reorder(State, Girls_Percentage),
    y = Girls_Percentage
  )
) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(
    title = "Girls' Share of Total School Enrolment",
    subtitle = "UDISE+ 2021-22",
    x = "State / UT",
    y = "Girls' Enrolment (%)"
  ) +
  theme_minimal()


# SCATTER PLOT

ggplot(
  literacy_analysis,
  aes(
    x = Male_Literacy,
    y = Female_Literacy
  )
) +
  geom_point(
    color = "steelblue",
    size = 3
  ) +
  geom_smooth(
    method = "lm",
    se = TRUE
  ) +
  labs(
    title = "Male vs Female Literacy Rate",
    x = "Male Literacy (%)",
    y = "Female Literacy (%)"
  ) +
  theme_minimal()


# LINE CHART

year_comparison <- tibble(
  Year = c("2020-21", "2021-22"),
  Total_Enrolment = c(
    sum(enrolment_2020$Total, na.rm = TRUE),
    sum(enrolment_2021$Total, na.rm = TRUE)
  )
)

ggplot(
  year_comparison,
  aes(
    x = Year,
    y = Total_Enrolment,
    group = 1
  )
) +
  geom_line(
    color = "blue",
    linewidth = 1.2
  ) +
  geom_point(size = 4) +
  labs(
    title = "School Enrolment Comparison",
    x = "Academic Year",
    y = "Total Enrolment"
  ) +
  theme_minimal()


# ==========================================
# ANALYSIS 5: SCHOOL INFRASTRUCTURE
# UDISE+ 2023-24
# ==========================================

# Check available columns

names(data_2023)

# Display first six records

head(data_2023)

# Check summary statistics

summary(data_2023)



# ==========================================
# ANALYSIS 5: SCHOOL INFRASTRUCTURE
# UDISE+ 2023-24
# ==========================================

# Select important columns

infrastructure <- data_2023 %>%
  select(
    State,
    Total_Schools = `Total Number of Schools`,
    Total_Enrolments = `Total Number of Enrolments`,
    Total_Teachers = `Total Number of Teachers`,
    PTR = `Pupil Teacher Ratio`,
    Avg_Teachers = `Average Teachers Per School`,
    Avg_Enrolments = `Average Enrolments Per School`,
    Zero_Enrolment_Schools = `Schools with Zero Enrolments`,
    Single_Teacher_Schools = `Schools with Single Teachers`
  )

# Remove India aggregate for state-level analysis

infrastructure <- infrastructure %>%
  filter(toupper(State) != "INDIA")

# Calculate additional indicators

infrastructure <- infrastructure %>%
  mutate(
    Zero_Enrolment_Percentage =
      (Zero_Enrolment_Schools / Total_Schools) * 100,
    
    Single_Teacher_Percentage =
      (Single_Teacher_Schools / Total_Schools) * 100
  )

# Display the prepared dataset

print(head(infrastructure))

# Check structure

str(infrastructure)



# Top 10 states by total schools

top_10_schools <- infrastructure %>%
  arrange(desc(Total_Schools)) %>%
  slice_head(n = 10)

print(top_10_schools)


# Highest pupil-teacher ratio

high_ptr <- infrastructure %>%
  arrange(desc(PTR)) %>%
  slice_head(n = 10)

print(high_ptr)



# States with the highest percentage
# of single-teacher schools

single_teacher_analysis <- infrastructure %>%
  arrange(desc(Single_Teacher_Percentage))

print(
  head(single_teacher_analysis, 10)
)



# BAR CHART: NUMBER OF SCHOOLS

ggplot(
  top_10_schools,
  aes(
    x = reorder(State, Total_Schools),
    y = Total_Schools
  )
) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(
    title = "Top 10 States by Number of Schools",
    subtitle = "UDISE+ 2023-24",
    x = "State / UT",
    y = "Number of Schools"
  ) +
  scale_y_continuous(labels = scales::comma) +
  theme_minimal()


# BAR CHART: PUPIL TEACHER RATIO

ggplot(
  high_ptr,
  aes(
    x = reorder(State, PTR),
    y = PTR
  )
) +
  geom_col(fill = "tomato") +
  coord_flip() +
  labs(
    title = "States with Highest Pupil-Teacher Ratio",
    subtitle = "UDISE+ 2023-24",
    x = "State / UT",
    y = "Students per Teacher"
  ) +
  theme_minimal()



# SINGLE TEACHER SCHOOL ANALYSIS

ggplot(
  infrastructure,
  aes(
    x = reorder(State, Single_Teacher_Percentage),
    y = Single_Teacher_Percentage
  )
) +
  geom_col(fill = "darkorange") +
  coord_flip() +
  labs(
    title = "Percentage of Single-Teacher Schools",
    subtitle = "UDISE+ 2023-24",
    x = "State / UT",
    y = "Single-Teacher Schools (%)"
  ) +
  theme_minimal()



# INFRASTRUCTURE SUMMARY

infrastructure_summary <- infrastructure %>%
  summarise(
    Total_Schools = sum(Total_Schools),
    Total_Students = sum(Total_Enrolments),
    Total_Teachers = sum(Total_Teachers),
    Average_PTR = mean(PTR),
    Total_Zero_Enrolment_Schools =
      sum(Zero_Enrolment_Schools),
    Total_Single_Teacher_Schools =
      sum(Single_Teacher_Schools)
  )

print(infrastructure_summary)

write_csv(
  infrastructure,
  "data/processed/infrastructure_analysis_2023_24.csv"
)

write_csv(
  infrastructure_summary,
  "data/processed/infrastructure_summary.csv"
)
