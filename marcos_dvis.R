library(readr)
library(dplyr)
library(ggplot2)
library(lubridate )
library(sf)
library(arrrow)

val_merged <- read_parquet("validations.parquet")

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

