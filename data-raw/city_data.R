## code to prepare `city_data` dataset goes here

# Wczytaj dane z CSV
city_data <- read.csv("city_optima_data.csv", sep = ";", stringsAsFactors = FALSE)

city_data$Cost_Living <- max(city_data$Cost_Living) - city_data$Cost_Living

usethis::use_data(city_data, overwrite = TRUE)
