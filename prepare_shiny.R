# -------------------------------
# Preprocessing Script - OPTIMIZED FOR MEMORY
# -------------------------------
library(dplyr)
library(arrow)
library(lubridate)
library(sf)

# Load validations
validations <- read_parquet("validations.parquet")

# Load station shapefile to get station names and type
zd <- st_read("zd.shp", quiet = TRUE) %>%
  st_set_geometry(NULL) %>%
  select(ID_REFA_LDA = idrefa_lda, nom_lda = nom, type_arret)

# Join validations with station info
validations <- validations %>%
  left_join(zd, by = "ID_REFA_LDA") %>%
  mutate(JOUR = as.Date(JOUR))

# -------------------------------
# Compute Norm (exclude holidays + COVID period)
# -------------------------------
covid_start <- as.Date("2020-03-01")
covid_end   <- as.Date("2022-06-30")

norm_data <- validations %>%
  filter(is_holiday == "Normal Day",
         !(JOUR >= covid_start & JOUR <= covid_end))

# Compute total validations per day across all stations
norm_daily_totals <- norm_data %>%
  group_by(JOUR, day_of_week) %>%
  summarise(total_validations = sum(NB_VALD, na.rm = TRUE), .groups = "drop")

# Compute average and SD per weekday
norm_by_weekday <- norm_daily_totals %>%
  group_by(day_of_week) %>%
  summarise(
    norm_avg = mean(total_validations, na.rm = TRUE),
    norm_sd  = sd(total_validations, na.rm = TRUE),
    .groups = "drop"
  )

write_parquet(norm_by_weekday, "ShinyApp/data/pre_aggregated/norm_by_weekday.parquet")

# -------------------------------
# Network-wide daily aggregates (for Compare with Norm, Period Comparison, Overall Trends)
# -------------------------------
daily_network <- validations %>%
  group_by(JOUR, day_of_week, is_holiday) %>%
  summarise(
    NB_VALD = sum(NB_VALD, na.rm = TRUE),
    .groups = "drop"
  )

write_parquet(daily_network, "ShinyApp/data/pre_aggregated/daily_network.parquet")

# -------------------------------
# Station-level daily data (for Station Explorer)
# -------------------------------
station_daily <- validations %>%
  group_by(JOUR, day_of_week, nom_lda, type_arret, CATEGORIE_TITRE) %>%
  summarise(total_validations = sum(NB_VALD, na.rm=TRUE), .groups="drop")

write_parquet(station_daily, "ShinyApp/data/pre_aggregated/station_daily.parquet")

# -------------------------------
# Station metadata (for Station Explorer info and dropdowns)
# -------------------------------
station_metadata <- validations %>%
  group_by(ID_REFA_LDA, nom_lda, type_arret) %>%
  summarise(
    total_validations = sum(NB_VALD, na.rm = TRUE),
    avg_daily = mean(NB_VALD, na.rm = TRUE),
    min_date = min(JOUR),
    max_date = max(JOUR),
    .groups = "drop"
  ) %>%
  arrange(desc(avg_daily))

write_parquet(station_metadata, "ShinyApp/data/pre_aggregated/station_metadata.parquet")

# -------------------------------
# Weekly aggregates (for Overall Trends)
# -------------------------------
weekly_network <- validations %>%
  mutate(week = floor_date(JOUR, "week")) %>%
  group_by(week) %>%
  summarise(total = sum(NB_VALD, na.rm = TRUE), .groups = "drop")

write_parquet(weekly_network, "ShinyApp/data/pre_aggregated/weekly_network.parquet")

# -------------------------------
# Mode totals (for Overall Trends)
# -------------------------------
mode_totals <- validations %>%
  filter(!is.na(type_arret)) %>%
  group_by(type_arret) %>%
  summarise(total = sum(NB_VALD, na.rm = TRUE), .groups = "drop")

write_parquet(mode_totals, "ShinyApp/data/pre_aggregated/mode_totals.parquet")

cat("Preprocessing complete! All files saved to ShinyApp/data/pre_aggregated/\n")
