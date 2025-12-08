library(readr)
library(dplyr)
library(ggplot2)
library(lubridate )
library(sf)

col_spec <- cols(.default = "c")

val_2025_first <- read_csv2("data/validations-reseau-ferre-nombre-validations-par-jour-1er-trimestre.csv", col_types = col_spec)
val_2025_second <- read_csv2("data/validations-reseau-ferre-nombre-validations-par-jour-2eme-trimestre.csv", col_types = col_spec)
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

val_2025_second <- rename(val_2025_second, ID_REFA_LDA = ID_ZDC)
val_2025_first <- rename(val_2025_first, ID_REFA_LDA = ID_ZDC)


########## Convert date here would be nice  d-m-y


val_2023_second <- rename(val_2023_second, ID_REFA_LDA = ID_ZDC)
val_2023_first <- rename(val_2023_first, ID_REFA_LDA = lda)
val_2024_first <- rename(val_2024_first, ID_REFA_LDA = ID_ZDC)
val_2022_second <- rename(val_2022_second, ID_REFA_LDA = lda)



zd <- st_read("data/REF_ZdA/PL_ZDL_R_11-11-2025.shp" )


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
  val_2025_first,
  val_2025_second
)

val_merged <- val_merged %>%
  mutate(JOUR = parse_date_time(JOUR, orders = c("ymd", "mdy", "dmy")))
val_merged$NB_VALD <- as.numeric(val_merged$NB_VALD)

zd <- zd |>
  filter(type_arret != "Arrêt de bus")

zd <- zd |> mutate(across(-geometry, as.character))

zd_grouped <- zd %>%
  group_by(idrefa_lda) %>%
  summarise(geometry = st_union(geometry))

val_zd <- left_join(val_merged, zd_grouped, by = c("ID_REFA_LDA" = "idrefa_lda"))
str(val_zd)

# group per year and season
df_season <- val_merged %>%
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
df_month <- val_merged %>%   
  mutate(
    date = as.Date(JOUR),
    year_month = floor_date(date, unit = "month")  
  ) %>%
  group_by(year_month) %>%
  summarise(
    NB_VALD = sum(NB_VALD, na.rm = TRUE),
    .groups = "drop"
  )



# covid and olimpic games
mean_val <- mean(df_month$NB_VALD, na.rm = TRUE)

# 
covid_start <- as.Date("2020-03-01")
covid_end   <- as.Date("2022-06-30")

olymp_start <- as.Date("2024-07-26")
olymp_end   <- as.Date("2024-08-11")

ggplot(df_month, aes(x = year_month, y = NB_VALD)) +
  # shadows  
  geom_rect(aes(xmin = covid_start, xmax = covid_end,
                ymin = -Inf, ymax = Inf),
            inherit.aes = FALSE, alpha = 0.2) +
  geom_rect(aes( xmin = olymp_start, xmax = olymp_end,
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
    subtitle = "Shadows = COVID, Olimpic Games; dashed = mean"
  ) +
  theme_classic()




