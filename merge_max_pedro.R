library(readr)
library(dplyr)
library(ggplot2)
library(lubridate)
library(sf)
library(scales)
library(arrow)

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
    "2025-12-25" # Noël
))
holidays_md <- format(holidays, "%m-%d")

validations = read_parquet("validations.parquet")

tot_val_per_day <- validations |>
  group_by(JOUR) |>
  summarise(NB_VALD=sum(NB_VALD), is_holiday=first(is_holiday))

avg_vals <- tot_val_per_day %>%
    group_by(is_holiday) %>%
    summarise(mean_val = mean(NB_VALD, na.rm = TRUE))


ggplot(tot_val_per_day, aes(x = is_holiday, y = NB_VALD, fill = is_holiday)) +
    geom_boxplot(show.legend = FALSE) +
    labs(
        title = "Validation Distribution: Holidays vs Normal Days",
        x = "",
        y = "Validation Count"
    ) +
    theme_minimal()

##############################################
######## Validations by type of pass
daily_totals <- validations %>%
  group_by(JOUR, day_of_week, CATEGORIE_TITRE) %>%
  summarise(total_val = sum(NB_VALD, na.rm = TRUE), .groups = "drop")

val_summary <- daily_totals %>%
  group_by(day_of_week, CATEGORIE_TITRE) %>%
  summarise(mean_val = mean(total_val, na.rm = TRUE), .groups = "drop")

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
########### Validations by weekday and type d'arret

zd = st_read("zd.shp")
zd <- zd %>%
  mutate(
    idrefa_lda = as.character(idrefa_lda),
    idrefa_lda = sub("\\.0$", "", idrefa_lda)   # correct order
  )
  
zd_lookup <- zd %>%
  st_drop_geometry() %>%
  select(idrefa_lda, type_arret) %>%
  distinct(idrefa_lda, .keep_all = TRUE)

val_joined_final <- validations |> 
  left_join(zd_lookup, by = c("ID_REFA_LDA" = "idrefa_lda"))

daily_totals <- val_joined_final %>%
  group_by(JOUR, day_of_week, type_arret) %>%
  summarise(total_val = sum(NB_VALD, na.rm = TRUE), .groups = "drop")

val_summary <- daily_totals %>%
    group_by(day_of_week, type_arret) %>%
    summarise(mean_val = mean(total_val, na.rm = TRUE), .groups = "drop")

ggplot(val_summary, aes(x = day_of_week, y = mean_val, fill = type_arret)) +
    geom_col(position = position_dodge(width = 0.8), width = 0.7) +
    scale_y_continuous(labels = scales::comma) +
    labs(title = "Validations by Mode", y = "Average Validations") +
    theme_minimal()

###############################
# BUSIEST WEEK DAY

weekday_stats <- validations %>%
  filter(is_holiday == "Normal Day") %>%
  # sum per day first
  group_by(JOUR, day_of_week) %>%
  summarise(daily_total = sum(NB_VALD, na.rm = TRUE), .groups = "drop") %>%
  # then average across weekdays
  group_by(day_of_week) %>%
  summarise(
    avg_daily_val = mean(daily_total, na.rm = TRUE),
    total_val = sum(daily_total, na.rm = TRUE),
    .groups = "drop"
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

# more general stats
Total_Ridership <- sum(validations$NB_VALD, na.rm = TRUE)
Total_Ridership
Total_Active_Stations <- n_distinct(validations$LIBELLE_ARRET)
Total_Active_Stations
Total_Days_Recorded <- n_distinct(validations$JOUR)
Total_Days_Recorded
Global_Daily_Avg <- sum(validations$NB_VALD, na.rm = TRUE) / n_distinct(validations$JOUR)
Global_Daily_Avg
daily_totals <- validations %>%
  group_by(JOUR) %>%
  summarise(daily_total = sum(NB_VALD, na.rm = TRUE), .groups = "drop")

# median across days
Global_Median_Daily <- median(daily_totals$daily_total, na.rm = TRUE)
Global_Median_Daily
#####################################################################
############### 1. GEOGRAPHICAL MAP - Average Validations ###########
#####################################################################

daily_validations <- validations %>%
  group_by(ID_REFA_LDA, JOUR) %>%
  summarise(daily_val = sum(NB_VALD, na.rm = TRUE), .groups = "drop")

# Step 2: Compute average per station over days
avg_validations_zone <- daily_validations %>%
  group_by(ID_REFA_LDA) %>%
  summarise(avg_validations = mean(daily_val, na.rm = TRUE), .groups = "drop")

zd_with_validation <- left_join(
  zd,
  avg_validations_zone,
  by = c("idrefa_lda" = "ID_REFA_LDA")
)

zd_filtered <- zd_with_validation %>%
  filter(!is.na(avg_validations))

cat("\n=== Data for visualization ===\n")
cat("Zones with validation data:", nrow(zd_filtered), "\n")

# Step 6: Create hex grid
hex_grid_geom <- st_make_grid(zd_filtered, cellsize = c(2000, 2000), what = "polygons", square = FALSE)
hex_grid <- st_sf(hex_id = 1:length(hex_grid_geom), geometry = hex_grid_geom)

# Step 7: Join stations to hex grid and aggregate
hex_validation <- st_join(hex_grid, zd_filtered) %>%
  group_by(hex_id) %>%
  summarise(
    avg_validations = mean(avg_validations, na.rm = TRUE),
    total_validations = sum(avg_validations, na.rm = TRUE),
    n_stations = sum(!is.na(avg_validations)),
    .groups = "drop"
  ) %>%
  filter(n_stations > 0)

cat("Hex cells with data:", nrow(hex_validation), "\n")

# Step 8: Plot
map_plot <- ggplot() +
  geom_sf(data = hex_validation, aes(fill = avg_validations), color = NA, alpha = 0.7) +
  scale_fill_viridis_c(
    option = "plasma",
    name = "Avg Validations\n(Area)",
    labels = comma,
    trans = "log10",
    breaks = c(10, 100, 1000, 10000),
    guide = guide_colorbar(order = 1)
  ) +
  labs(
    title = "Average Daily Validations - Paris Transit Network (Q1 2025)",
    subtitle = "Hexagonal heatmap for grouping (2km cells)"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5, margin = margin(b = 5)),
    plot.subtitle = element_text(size = 11, hjust = 0.5, color = "grey40", margin = margin(b = 10)),
    legend.position = "right",
    legend.title = element_text(size = 10, face = "bold"),
    legend.text = element_text(size = 8),
    plot.margin = margin(15, 15, 15, 15),
    plot.background = element_rect(fill = "white", color = NA)
  )
map_plot

#####################################################################
############### 2. TIME EVOLUTION - Line Plot #######################
#####################################################################
weekly_validations <- validations %>%
  group_by(week = floor_date(JOUR, unit = "week")) %>%
  summarise(
    total_validations = sum(NB_VALD, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  # remove first and last week
  filter(week != min(week) & week != max(week))

ggplot(weekly_validations, aes(x = week, y = total_validations)) +
  geom_line(color = "#2C3E50", linewidth = 0.8) +
  scale_y_continuous(labels = comma) +
  labs(
    title = "Weekly Total Validations",
    x = "Week",
    y = "Total Validations"
  ) +
  theme_minimal()

#####################################################################
############### 3. DISTRIBUTION per Station/Zone ####################
#####################################################################

# Step 1: Sum validations per station per day
daily_station_vals <- validations %>%
  group_by(ID_REFA_LDA, JOUR) %>%
  summarise(daily_val = sum(NB_VALD, na.rm = TRUE), .groups = "drop")

# Step 2: Compute average daily validations per station
station_avg_validations <- daily_station_vals %>%
  group_by(ID_REFA_LDA) %>%
  summarise(avg_daily_validations = mean(daily_val, na.rm = TRUE), .groups = "drop") %>%
  # Join with zd to get station names
  left_join(
    zd %>% st_drop_geometry() %>% 
      select(ID_REFA_LDA = idrefa_lda, nom_lda) %>% 
      distinct(ID_REFA_LDA, .keep_all = TRUE), 
    by = "ID_REFA_LDA"
  ) %>%
  arrange(desc(avg_daily_validations))

# Step 3: Keep top 30 stations
top_stations <- station_avg_validations %>%
  slice_max(avg_daily_validations, n = 30) %>%
  # remove any remaining duplicates (just in case)
  distinct(ID_REFA_LDA, .keep_all = TRUE)

# Step 4: Plot with descending order
dist_plot <- ggplot(top_stations, aes(
  x = reorder(nom_lda, avg_daily_validations),  # reorder by avg daily validations
  y = avg_daily_validations
)) +
  geom_col(fill = "#3498DB") +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Top 30 Stations by Average Daily Validations",
    x = "Station Name",
    y = "Average Daily Validations"
  ) +
  coord_flip() +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.text.y = element_text(size = 9)
  )
dist_plot

###############################################################
####

# group per year and season
df_season <- validations %>%
  mutate(
    date  = as.Date(JOUR),
    year  = year(date),
    md    = month(date) * 100 + day(date),  
    season = case_when(
      md >= 322  & md < 621  ~ "Spring", # 22/03 to 20/06
      md >= 621  & md < 923  ~ "Summer",     # 21/06 to 22/09
      md >= 923  & md < 1222 ~ "Fall",    # 23/09 to 21/12
      TRUE                    ~ "Winter"   # 22/12 to 21/03
    )
  ) %>%
  group_by(year, season) %>%
  summarise(NB_VALD = sum(NB_VALD, na.rm = TRUE), .groups = "drop") %>%
  mutate(
    season = factor(season, levels = c("Spring","Summer","Fall","Winter"))
  )

head(df_season)

df_mean_year <- df_season %>%
  group_by(year) %>%
  summarise(mean_year = mean(NB_VALD, na.rm = TRUE), .groups = "drop")


ggplot(df_season, aes(x = factor(year), y = NB_VALD, fill = season)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  geom_line(
    data = df_mean_year,
    aes(x = factor(year), y = mean_year, group = 1),
    inherit.aes = FALSE,
    linewidth = 1
  ) +
  geom_point(
    data = df_mean_year,
    aes(x = factor(year), y = mean_year),
    inherit.aes = FALSE,
    size = 2
  ) +
  labs(
    x = "Year",
    y = "Total validation (NB_VALD)",
    fill = "Season",
    title = "Validations per year per season + annual mean"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# validation per month
df_month <- validations %>%   
  mutate(
    date = as.Date(JOUR),
    year_month = floor_date(date, unit = "month")  
  ) %>%
  group_by(year_month) %>%
  summarise(
    NB_VALD = sum(NB_VALD, na.rm = TRUE),
    .groups = "drop"
  )



# covid 
mean_val <- mean(df_month$NB_VALD, na.rm = TRUE)

# 
covid_start <- as.Date("2020-03-01")
covid_end   <- as.Date("2022-06-30")

ggplot(df_month, aes(x = year_month, y = NB_VALD)) +
  # shadows  
  geom_rect(aes(xmin = covid_start, xmax = covid_end,
                ymin = -Inf, ymax = Inf),
            inherit.aes = FALSE, alpha = 0.2) +
  # validations area
  geom_area( fill  ="#3498DB",  alpha = 0.5) +
  # mean
  geom_hline(yintercept = mean_val, linetype = "dashed", linewidth = 0.8) +
  
  labs(
    x = "Month",
    y = "Validations (NB_VALD)",
    title = "Validations per Month",
    subtitle = "Shadows = COVID; dashed = mean"
  ) +
  theme_classic()

