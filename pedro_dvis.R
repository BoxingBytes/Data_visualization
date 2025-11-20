library(readr)
library(dplyr)
library(ggplot2)
library(lubridate )
library(sf)

#######################
col_spec <- cols(.default = "c")

val_2025_first <- read_csv2("data/validations-reseau-ferre-nombre-validations-par-jour-1er-trimestre.csv", col_types = col_spec)

val_explore <- val_2025_first
val_2025_first <- rename(val_2025_first, ID_REFA_LDA = ID_ZDC)



val_explore <- val_2025_first



zd <- st_read("data/REF_ZdA/PL_ZDL_R_11-11-2025.shp" )

zd <- zd |> mutate(across(-geometry, as.character))
str(zd)

zd_grouped <- zd %>%
  group_by(idrefa_lda) %>%
  summarise(geometry = st_union(geometry))




val_zd <- left_join(val_explore, zd_grouped, by = c("ID_REFA_LDA" = "idrefa_lda"))


val_explore <- val_explore |> mutate(day_of_week = wday(JOUR, label = T))
val_explore$NB_VALD <- as.numeric(val_explore$NB_VALD)

###

holidays <- as.Date(c(
  "2025-01-01", # Jour de l'An
  "2025-04-21", # Lundi de Pâques
  "2025-05-01", # Fête du Travail
  "2025-05-08", # Armistice
  "2025-05-29", # Ascension
  "2025-06-09", # Lundi de Pentecôte
  "2025-07-14", # Fête Nationale
  "2025-08-15", # Assomption
  "2025-11-01", # Toussaint
  "2025-11-11", # Armistice
  "2025-12-25"  # Noël
))


val_explore <- val_explore %>%
  mutate(
    is_holiday = JOUR %in% holidays,
    is_holiday = ifelse(is_holiday, "Holiday", "Normal Day")
  )

avg_vals <- val_explore %>%
  group_by(is_holiday) %>%
  summarise(mean_val = mean(NB_VALD, na.rm = TRUE))


ggplot(val_explore, aes(x = is_holiday, y = NB_VALD, fill = is_holiday)) +
  geom_boxplot(show.legend = FALSE) +
  labs(
    title = "Validation Distribution: Holidays vs Normal Days",
    x = "",
    y = "Validation Count"
  ) +
  theme_minimal()

##############################################
########Validations by type of pass

val_summary <- val_explore %>%
  group_by(day_of_week, CATEGORIE_TITRE) %>%
  summarise(mean_val = mean(NB_VALD, na.rm = TRUE))

ggplot(val_summary, aes(x = day_of_week, y = mean_val, fill = CATEGORIE_TITRE)) +
  geom_col(position = "dodge") +
  labs(
    title = "Average Validations by Weekday and Category",
    x = "Day of Week",
    y = "Average Validations",
    fill = "Category"
  ) +
  theme_minimal()

#################################################

zd_lookup <- zd %>%
  st_drop_geometry() %>% 
  select(idrefa_lda, type_arret) %>% 
  mutate(
    join_id = as.numeric(idrefa_lda) 
  ) %>%
  distinct(join_id, .keep_all = TRUE)

val_explore_clean <- val_explore %>%
  mutate(
    join_id = as.numeric(ID_REFA_LDA) 
  )

val_joined_final <- inner_join(val_explore_clean, zd_lookup, by = "join_id")

val_summary <- val_joined_final %>%
  group_by(day_of_week, type_arret) %>%
  summarise(mean_val = mean(NB_VALD, na.rm = TRUE), .groups = "drop")

ggplot(val_summary, aes(x = day_of_week, y = mean_val, fill = type_arret)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Validations by Mode", y = "Average Validations") +
  theme_minimal()

###############################

#GENERAL STATS
top_stations <- val_explore %>%
  group_by(LIBELLE_ARRET) %>%
  summarise(
    total_validations = sum(NB_VALD, na.rm = TRUE),
    avg_daily = mean(NB_VALD, na.rm = TRUE)
  ) %>%
  arrange(desc(total_validations)) %>%
  head(10)

ggplot(top_stations, aes(x = reorder(LIBELLE_ARRET, total_validations), y = total_validations)) +
  geom_col(fill = "steelblue") +
  coord_flip() + 
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Top 10 Busiest Stations", x = "", y = "Total Validations") +
  theme_minimal()

#BUSIEST WEEK DAY

weekday_stats <- val_explore %>%
  filter(is_holiday == "Normal Day") %>% 
  group_by(day_of_week) %>%
  summarise(
    avg_daily_val = mean(NB_VALD, na.rm = TRUE),
    total_val = sum(NB_VALD, na.rm = TRUE)
  ) %>%
  arrange(desc(avg_daily_val)) 

ggplot(weekday_stats, aes(x = reorder(day_of_week, -avg_daily_val), y = avg_daily_val)) +
  geom_col(fill = "darkblue") +
  geom_text(aes(label = round(avg_daily_val, 0)), vjust = -0.5, size = 3.5) +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Average Traffic by Day of Week",
    subtitle = "Excluding Holidays",
    x = "Day",
    y = "Average Validations"
  ) +
  theme_minimal()

#more general stats
Total_Ridership = sum(val_explore$NB_VALD, na.rm = TRUE)
Total_Ridership
Total_Active_Stations = n_distinct(val_explore$LIBELLE_ARRET)
Total_Active_Stations
Total_Days_Recorded = n_distinct(val_explore$JOUR)
Total_Days_Recorded
Global_Daily_Avg = sum(val_explore$NB_VALD, na.rm = TRUE) / n_distinct(val_explore$JOUR)
Global_Daily_Avg
Global_Median_Daily = median(val_explore$NB_VALD, na.rm = TRUE)
Global_Median_Daily
