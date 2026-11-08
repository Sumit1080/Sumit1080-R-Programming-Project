
# Missing values in all datasets

cat("Missing values - 2020-21:\n")
colSums(is.na(data_2020))

cat("Missing values - 2021-22:\n")
colSums(is.na(data_2021))

cat("Missing values - 2023-24:\n")
colSums(is.na(data_2023))

cat("Missing values - Census:\n")
colSums(is.na(literacy))


# Check duplicate rows

sum(duplicated(data_2020))
sum(duplicated(data_2021))
sum(duplicated(data_2023))
sum(duplicated(literacy))

#Data Types
str(data_2020)
str(data_2021)
str(data_2023)
str(literacy)

#Summary Statistics
summary(data_2020)
summary(data_2021)
summary(data_2023)
summary(literacy)

#Dataset Size
cat("2020-21:", nrow(data_2020), "rows\n")
cat("2021-22:", nrow(data_2021), "rows\n")
cat("2023-24:", nrow(data_2023), "rows\n")
cat("Census:", nrow(literacy), "rows\n")
