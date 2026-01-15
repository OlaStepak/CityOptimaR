# Wczytaj surowy CSV
city_data <- read.csv("city_optima_data.csv", sep = ";", stringsAsFactors = FALSE)

# (Opcjonalnie) przekształcenie kryteriów, np. Cost_Living: mniejsze = lepsze
city_data$Cost_Living <- max(city_data$Cost_Living) - city_data$Cost_Living

# Zapisz dane do folderu data/ w formacie .rda
# Upewnij się, że masz zainstalowany pakiet usethis
usethis::use_data(city_data, overwrite = TRUE)
