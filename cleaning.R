library(readr)
library(dplyr)
library(ggplot2)
library(lubridate )
library(sf)
library(arrow)
library(sfarrow)




#####################################################################
################ 1. Data Collection and Cleaning ####################
#####################################################################

col_spec <- cols(.default = "c")

val_2018_first <- read_delim("data/data-rf-2018/2018_S1_NB_FER.txt", delim = "\t", col_types = col_spec)
val_2018_second <- read_delim("data/data-rf-2018/2018_S2_NB_FER.txt", delim = "\t",col_types = col_spec)
val_2019_first <- read_delim("data/data-rf-2019/2019_S1_NB_FER.txt", delim = "\t",col_types = col_spec)
val_2019_second <- read_delim("data/data-rf-2019/2019_S2_NB_FER.txt", delim = "\t",col_types = col_spec)
val_2020_first <- read_delim("data/data-rf-2020/2020_S1_NB_FER.txt", delim = "\t",col_types = col_spec)
val_2020_second <- read_delim("data/data-rf-2020/2020_S2_NB_FER.txt", delim = "\t",col_types = col_spec)
val_2021_first <- read_delim("data/data-rf-2021/2021_S1_NB_FER.txt", delim = "\t", col_types = col_spec)
val_2021_second <- read_delim("data/data-rf-2021/2021_S2_NB_FER.txt", delim = "\t", col_types = col_spec)
val_2022_first <- read_delim("data/data-rf-2022/2022_S1_NB_FER.txt", delim = "\t", col_types = col_spec)
val_2022_second <- read_delim("data/data-rf-2022/2022_S2_NB_FER.txt", delim = ";", col_types = col_spec)
val_2023_first <- read_delim("data/data-rf-2023/2023_S1_NB_FER.txt", delim = "\t", col_types = col_spec)
val_2023_second <- read_delim("data/data-rf-2023/2023_S2_NB_FER.txt", delim = "\t", locale = locale(encoding = "UTF-16"), col_types = col_spec)
val_2024_first <- read_delim("data/data-rf-2024/2024_S1_NB_FER.txt", delim = "\t", col_types = col_spec)





val_2023_second <- rename(val_2023_second, ID_REFA_LDA = ID_ZDC)
val_2023_first <- rename(val_2023_first, ID_REFA_LDA = lda)
val_2024_first <- rename(val_2024_first, ID_REFA_LDA = ID_ZDC)
val_2022_second <- rename(val_2022_second, ID_REFA_LDA = lda)



val_merged <- bind_rows(
  val_2018_first,
  val_2018_second,
  val_2019_first,
  val_2019_second,
  val_2020_first,
  val_2020_second,
  val_2021_first,
  val_2021_second,
  val_2022_first,
  val_2022_second,
  val_2023_first,
  val_2023_second,
  val_2024_first,
)


val_merged <- val_merged |> na.omit()

val_merged <- val_merged %>%
  mutate(JOUR = parse_date_time(JOUR, orders = c("ymd", "mdy", "dmy")))


val_merged$NB_VALD = as.integer(val_merged$NB_VALD) 

val_merged_cast <- val_merged %>% mutate(ID_REFA_LDA = sub("\\.0$", "", ID_REFA_LDA))


val_grouped = val_merged_cast |> group_by(ID_REFA_LDA,JOUR,CATEGORIE_TITRE) |> summarise(NB_VALD = sum(NB_VALD))


write_parquet(val_grouped, "validations.parquet")

#load zone de arret
zd <- st_read("data/REF_ZdA/PL_ZDL_R_11-11-2025.shp" )



#The documentation available at the official portal
#states that Arret debus and Arret de tram are not contained in the validation files above loaded
zd_clean = zd |> filter(!type_arret %in% c('Arrêt de bus','Arrêt de tram'))

#get the stations of all validations
unique_lda = unique(val_grouped$ID_REFA_LDA)

unique_lda

#Crosscheck to see if every station contained in validations, still is available on ZoneDeArret
all( unique_lda %in% zd_clean$idrefa_lda)

filtered <- setdiff(unique_lda, zd_clean$idrefa_lda)
filtered

dim(filtered)


length(unique_lda)
length(zd_clean$idrefa_lda)

zd_clean_only_val_stations = zd_clean |> filter(idrefa_lda %in% unique_lda)

dim(zd_clean_only_val_stations)



str(zd_clean_only_val_stations)

#
length(unique(val_merged_cast$ID_REFA_LDA))
length(unique(zd_clean_only_val_stations$idrefa_lda))

# elements that are in the first and not in the second
diff = setdiff(unique(val_merged_cast$ID_REFA_LDA), unique(zd_clean_only_val_stations$idrefa_lda))
diff

# find the diff at the original ZD
zd_missing_rows <- zd %>% filter(idrefa_lda %in% diff)
zd_missing_rows

dim(zd_missing_rows)

#could we just remove the missing zd_missing_rows from the merged dataset, as we doent need 

# Find IDs in val_merged_cast that are NOT in the reference dataset
ids_to_remove <- setdiff(unique(val_merged_cast$ID_REFA_LDA), 
                         unique(zd_clean_only_val_stations$idrefa_lda))

# Keep rows where ID is NOT (!) in the removal list
val_merged_clean <- val_merged_cast[ !val_merged_cast$ID_REFA_LDA %in% ids_to_remove, ]


length(unique(val_merged_clean$ID_REFA_LDA))
length(unique(zd_clean_only_val_stations$idrefa_lda))

dim(val_merged_clean)
dim(zd_clean_only_val_stations)



# unifying the types of CATEGORIE_TITRE PAG. 21 from Données de validation télébilléttiques
val_merged_clean <- val_merged_clean %>% mutate(CATEGORIE_TITRE = if_else(CATEGORIE_TITRE == "Imagine R", "IMAGINE R", CATEGORIE_TITRE))
val_merged_clean <- val_merged_clean %>% mutate(CATEGORIE_TITRE = if_else(CATEGORIE_TITRE == "NAVIGO JOUR", "NAVIGO", CATEGORIE_TITRE))
val_merged_clean <- val_merged_clean %>% mutate(CATEGORIE_TITRE = if_else(CATEGORIE_TITRE == "Amethyste", "AMETHYSTE", CATEGORIE_TITRE))
val_merged_clean <- val_merged_clean %>%
  mutate(CATEGORIE_TITRE = if_else(CATEGORIE_TITRE %in% c("Contrat Solidarité Transport", "Contrat Solidarit\xe9 Transport" ), "TST", CATEGORIE_TITRE))
val_merged_clean <- val_merged_clean %>%
  mutate(CATEGORIE_TITRE = if_else(CATEGORIE_TITRE %in% c("Forfait Navigo",   "Forfaits courts" ), "FGT", CATEGORIE_TITRE))
val_merged_clean <- val_merged_clean %>%
  mutate(CATEGORIE_TITRE = if_else(CATEGORIE_TITRE == "Autres titres", "AUTRE TITRE", CATEGORIE_TITRE))
val_merged_clean <- val_merged_clean %>%
  mutate(CATEGORIE_TITRE = if_else(CATEGORIE_TITRE == "?", "NON DEFINI", CATEGORIE_TITRE))

dim(val_merged_clean)
table(val_merged_clean$CATEGORIE_TITRE)


write_parquet(val_merged_clean, "validations.parquet")
st_write(zd_clean_only_val_stations, "zd.shp")


# -----------------------------------------------------------------
# Specific aggregations for the shiny app. Not required for eda
# -----------------------------------------------------------------

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



