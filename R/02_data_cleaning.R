
# ==========================================
# EDUCATION EQUALITY IN INDIA
# PHASE 3: DATA CLEANING
# ==========================================

library(tidyverse)

# Import all four datasets

data_2020 <- read_csv(
  "data/raw/UDISE_2020_21_Table_3.17.csv"
)

data_2021 <- read_csv(
  "data/raw/UDISE_2021_22_Table_5.17_2.csv"
)

data_2023 <- read_csv(
  "data/raw/udise_2023_24.csv"
)

literacy <- read_csv(
  "data/raw/literacy_census_2011.csv"
)

#To check missing values
# Count missing values in each dataset

sum(is.na(data_2020))
sum(is.na(data_2021))
sum(is.na(data_2023))
sum(is.na(literacy))

#Remove duplicates values
data_2020 <- distinct(data_2020)
data_2021 <- distinct(data_2021)
data_2023 <- distinct(data_2023)
literacy <- distinct(literacy)

# Clean text values

data_2020 <- data_2020 %>%
  mutate(across(where(is.character), str_trim))

data_2021 <- data_2021 %>%
  mutate(across(where(is.character), str_trim))

data_2023 <- data_2023 %>%
  mutate(across(where(is.character), str_trim))

literacy <- literacy %>%
  mutate(across(where(is.character), str_trim))


data_2020 <- data_2020 %>%
  rename(State = `India/State/ UT`)

data_2021 <- data_2021 %>%
  rename(State = `India/State/ UT`)

data_2023 <- data_2023 %>%
  rename(State = `India/State/UT`)

literacy <- literacy %>%
  rename(State = `State / UT`)


# Check dimensions

dim(data_2020)
dim(data_2021)
dim(data_2023)
dim(literacy)

# Check duplicate records

sum(duplicated(data_2020))
sum(duplicated(data_2021))
sum(duplicated(data_2023))
sum(duplicated(literacy))

# Check missing values

sum(is.na(data_2020))
sum(is.na(data_2021))
sum(is.na(data_2023))
sum(is.na(literacy))



#Save Cleaned Datasets

# Save UDISE 2020-21

write_csv(
  data_2020,
  "data/processed/cleaned_2020_21.csv"
)

# Save UDISE 2021-22

write_csv(
  data_2021,
  "data/processed/cleaned_2021_22.csv"
)

# Save UDISE 2023-24

write_csv(
  data_2023,
  "data/processed/cleaned_2023_24.csv"
)

# Save Census Literacy 2011

write_csv(
  literacy,
  "data/processed/cleaned_literacy_2011.csv"
)

# Verify saved files

list.files("data/processed")

cat("All cleaned datasets saved successfully!")

