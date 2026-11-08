
# ==========================================
# EDUCATION EQUALITY IN INDIA
# PHASE 2: DATA IMPORT
# ==========================================

# Load libraries
library(tidyverse)

# 1. Import datasets

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

# 2. Check dimensions

cat("2020-21 Dataset:\n")
print(dim(data_2020))

cat("2021-22 Dataset:\n")
print(dim(data_2021))

cat("2023-24 Dataset:\n")
print(dim(data_2023))

cat("Census Literacy Dataset:\n")
print(dim(literacy))

# 3. Display first six rows

head(data_2020)
head(data_2021)
head(data_2023)
head(literacy)

# 4. Check dataset structure

str(data_2020)
str(data_2021)
str(data_2023)
str(literacy)

# 5. Display column names

names(data_2020)
names(data_2021)
names(data_2023)
names(literacy)
