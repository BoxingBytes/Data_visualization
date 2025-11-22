library(readr)
library(dplyr)
library(ggplot2)
library(lubridate)
library(sf)
library(scales)  

col_spec <- cols(.default = "c")
val_2025_first <- read_csv2("data/validations-reseau-ferre-nombre-validations-par-jour-1er-trimestre.csv", 
                             col_types = col_spec)
val_2025_first <- rename(val_2025_first, ID_REFA_LDA = ID_ZDC)

zd <- st_read("data/REF_ZdA/PL_ZDL_R_11-11-2025.shp")
zd <- zd |> mutate(across(-geometry, as.character))

val_explore <- val_2025_first |>
    mutate(
        JOUR = parse_date_time(JOUR, orders = c("ymd", "mdy", "dmy")),
        NB_VALD = as.numeric(NB_VALD)
    ) |>
    na.omit()

#####################################################################
############### 1. GEOGRAPHICAL MAP - Average Validations ###########
#####################################################################

avg_validations_zone <- val_explore |>
    group_by(ID_REFA_LDA, LIBELLE_ARRET) |>
    summarise(avg_validations = mean(NB_VALD, na.rm = TRUE), .groups = "drop") |>
    mutate(ID_REFA_LDA = as.character(as.integer(as.numeric(ID_REFA_LDA))))

zd_with_ids <- zd |>
    mutate(idrefa_lda_clean = as.character(as.integer(as.numeric(idrefa_lda))))

zd_with_validation <- left_join(zd_with_ids, avg_validations_zone, 
                                 by = c("idrefa_lda_clean" = "ID_REFA_LDA"))

# Filter to only zones with data
zd_filtered <- zd_with_validation |>
    filter(!is.na(avg_validations))

cat("\n=== Data for visualization ===\n")
cat("Zones with validation data:", nrow(zd_filtered), "\n")

# LLM solution to get hex grid grouping otherwise celles too small
hex_grid <- st_make_grid(zd_filtered, cellsize = c(2000, 2000), what = "polygons", square = FALSE) |>
    st_sf(hex_id = 1:length(.))

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
    geom_sf(data = hex_validation, aes(fill = avg_validations), 
            color = NA, alpha = 0.7) +
    scale_fill_viridis_c(option = "plasma", 
                        name = "Avg Validations\n(Area)",
                        labels = comma,
                        trans = "log10",
                        breaks = c(10, 100, 1000, 10000),
                        guide = guide_colorbar(order = 1)) +
    labs(title = "Average Daily Validations - Paris Transit Network (Q1 2025)",
        subtitle = "Hexagonal heatmap for grouping (2km cells)") +
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
        is_weekend = wday(JOUR) %in% c(1, 7)  # Sunday=1, Saturday=7
    )

print(head(daily_validations, 20))

time_plot <- ggplot(daily_validations, aes(x = JOUR, y = total_validations)) +
    geom_line(color = "#2C3E50", linewidth = 0.8) +
    geom_point(aes(color = is_weekend), size = 2.5) +
    scale_y_continuous(labels = comma) +
    scale_color_manual(values = c("FALSE" = "#3498DB", "TRUE" = "#E74C3C"),
                        labels = c("FALSE" = "Weekday", "TRUE" = "Weekend"),
                        name = "Day Type") +
    labs(title = "Time Evolution of Daily Validations (Q1 2025)",
        subtitle = "Weekends shown in red, weekdays in blue",
        x = "Date",
        y = "Total Daily Validations") +
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

dist_plot <- ggplot(top_stations, aes(x = reorder(LIBELLE_ARRET, avg_daily_validations), 
                                       y = avg_daily_validations)) +
    geom_col(fill = "#3498DB") +
    scale_y_continuous(labels = comma) +
    labs(title = "Top 30 Stations by Average Daily Validations (Q1 2025)",
        x = "Station Name",
        y = "Average Daily Validations") +
    coord_flip() +
    theme_minimal() +
    theme(
        plot.title = element_text(face = "bold", size = 14),
        axis.text.y = element_text(size = 9)
    )

print(dist_plot)
ggsave("distribution_validations.png", dist_plot, width = 10, height = 10, dpi = 300)