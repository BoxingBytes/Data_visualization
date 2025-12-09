library(readr)
library(dplyr)
library(ggplot2)
library(lubridate)
library(sf)
library(scales)


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
    "2025-12-25" # Noël
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
######## Validations by type of pass

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
# BUSIEST WEEK DAY

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

# more general stats
Total_Ridership <- sum(val_explore$NB_VALD, na.rm = TRUE)
Total_Ridership
Total_Active_Stations <- n_distinct(val_explore$LIBELLE_ARRET)
Total_Active_Stations
Total_Days_Recorded <- n_distinct(val_explore$JOUR)
Total_Days_Recorded
Global_Daily_Avg <- sum(val_explore$NB_VALD, na.rm = TRUE) / n_distinct(val_explore$JOUR)
Global_Daily_Avg
Global_Median_Daily <- median(val_explore$NB_VALD, na.rm = TRUE)
Global_Median_Daily

#####################################################################
############### 1. GEOGRAPHICAL MAP - Average Validations ###########
#####################################################################

val_explore <- val_2025_first |>
    mutate(
        JOUR = parse_date_time(JOUR, orders = c("ymd", "mdy", "dmy")),
        NB_VALD = as.numeric(NB_VALD)
    ) |>
    na.omit()

avg_validations_zone <- val_explore |>
    group_by(ID_REFA_LDA, LIBELLE_ARRET) |>
    summarise(avg_validations = mean(NB_VALD, na.rm = TRUE), .groups = "drop") |>
    mutate(ID_REFA_LDA = as.character(as.integer(as.numeric(ID_REFA_LDA))))

zd_with_ids <- zd |>
    mutate(idrefa_lda_clean = as.character(as.integer(as.numeric(idrefa_lda))))

zd_with_validation <- left_join(zd_with_ids, avg_validations_zone,
    by = c("idrefa_lda_clean" = "ID_REFA_LDA")
)

# Filter to only zones with data
zd_filtered <- zd_with_validation |>
    filter(!is.na(avg_validations))

cat("\n=== Data for visualization ===\n")
cat("Zones with validation data:", nrow(zd_filtered), "\n")

# LLM solution to get hex grid grouping otherwise celles too small
hex_grid_geom <- st_make_grid(zd_filtered, cellsize = c(2000, 2000), what = "polygons", square = FALSE)
hex_grid <- st_sf(hex_id = 1:length(hex_grid_geom), geometry = hex_grid_geom)

hex_validation <- st_join(hex_grid, zd_filtered) |>
    group_by(hex_id) |>
    summarise(
        avg_validations = mean(avg_validations, na.rm = TRUE),
        total_validations = sum(avg_validations, na.rm = TRUE),
        n_stations = sum(!is.na(avg_validations)),
        .groups = "drop"
    ) |>
    filter(n_stations > 0)

cat("Hex cells with data:", nrow(hex_validation), "\n")

map_plot <- ggplot() +
    geom_sf(
        data = hex_validation, aes(fill = avg_validations),
        color = NA, alpha = 0.7
    ) +
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

print(map_plot)
ggsave("map_avg_validations.png", map_plot, width = 10, height = 8, dpi = 300)

#####################################################################
############### 2. TIME EVOLUTION - Line Plot #######################
#####################################################################

daily_validations <- val_explore |>
    group_by(JOUR) |>
    summarise(total_validations = sum(NB_VALD, na.rm = TRUE)) |>
    arrange(JOUR)

daily_validations <- daily_validations |>
    mutate(
        day_of_week = wday(JOUR, label = TRUE),
        is_weekend = wday(JOUR) %in% c(1, 7) # Sunday=1, Saturday=7
    )

print(head(daily_validations, 20))

time_plot <- ggplot(daily_validations, aes(x = JOUR, y = total_validations)) +
    geom_line(color = "#2C3E50", linewidth = 0.8) +
    geom_point(aes(color = is_weekend), size = 2.5) +
    scale_y_continuous(labels = comma) +
    scale_color_manual(
        values = c("FALSE" = "#3498DB", "TRUE" = "#E74C3C"),
        labels = c("FALSE" = "Weekday", "TRUE" = "Weekend"),
        name = "Day Type"
    ) +
    labs(
        title = "Time Evolution of Daily Validations (Q1 2025)",
        subtitle = "Weekends shown in red, weekdays in blue",
        x = "Date",
        y = "Total Daily Validations"
    ) +
    theme_minimal() +
    theme(
        plot.title = element_text(face = "bold", size = 14),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "right"
    )

print(time_plot)
ggsave("time_evolution_validations.png", time_plot, width = 12, height = 6, dpi = 300)

#####################################################################
############### 3. DISTRIBUTION per Station/Zone ####################
#####################################################################

station_avg_validations <- val_explore |>
    group_by(ID_REFA_LDA, LIBELLE_ARRET) |>
    summarise(avg_daily_validations = mean(NB_VALD, na.rm = TRUE), .groups = "drop") |>
    arrange(desc(avg_daily_validations))

top_stations <- station_avg_validations |>
    top_n(30, avg_daily_validations)

dist_plot <- ggplot(top_stations, aes(
    x = reorder(LIBELLE_ARRET, avg_daily_validations),
    y = avg_daily_validations
)) +
    geom_col(fill = "#3498DB") +
    scale_y_continuous(labels = comma) +
    labs(
        title = "Top 30 Stations by Average Daily Validations (Q1 2025)",
        x = "Station Name",
        y = "Average Daily Validations"
    ) +
    coord_flip() +
    theme_minimal() +
    theme(
        plot.title = element_text(face = "bold", size = 14),
        axis.text.y = element_text(size = 9)
    )

print(dist_plot)
ggsave("distribution_validations.png", dist_plot, width = 10, height = 10, dpi = 300)
