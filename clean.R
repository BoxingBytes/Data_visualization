library(readr)
library(dplyr)
library(ggplot2)
library(lubridate )
library(sf)



#####################################################################
################ 1. Data Collection and Cleaning ####################
#####################################################################

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


val_merged <- val_merged |> na.omit()

val_merged <- val_merged %>%
  mutate(JOUR = parse_date_time(JOUR, orders = c("ymd", "mdy", "dmy")))


# 6,726 more rows with NA but there are all empty so we just remove them 
# na_df <- val_merged[!complete.cases(val_merged),]
# write.csv(na_df, "data/val_merged_na.csv")




zd <- st_read("data/REF_ZdA/PL_ZDL_R_11-11-2025.shp" )

zd <- zd |> mutate(across(-geometry, as.character))
str(zd)

zd_grouped <- zd %>%
  group_by(idrefa_lda) %>%
  summarise(geometry = st_union(geometry))



val_zd <- left_join(val_merged, zd_grouped, by = c("ID_REFA_LDA" = "idrefa_lda"))
#st_write(val_zd,"val_zd.shp")


#####################################################################
############### 2. Exploratory Data Analysis (EDA) ##################
#####################################################################

val_explore <- val_zd

##############################REMOVE AFTER

val_explore <- val_2025_first
val_2025_first <- rename(val_2025_first, ID_REFA_LDA = ID_ZDC)



zd <- st_read("data/REF_ZdA/PL_ZDL_R_11-11-2025.shp" )

zd <- zd |> mutate(across(-geometry, as.character))
str(zd)

zd_grouped <- zd %>%
  group_by(idrefa_lda) %>%
  summarise(geometry = st_union(geometry))




val_zd <- left_join(val_explore, zd_grouped, by = c("ID_REFA_LDA" = "idrefa_lda"))





##############################

val
val_explore <- val_explore |> mutate(day_of_week = wday(JOUR, label = T))
val_explore$NB_VALD <- as.numeric(val_explore$NB_VALD)
val_bplot <- ggplot(val_explore, aes(x=day_of_week, y = NB_VALD)) + geom_boxplot() + labs(title = "Distribution of Validations per Week Day)",x = "Day of Week", y = "Validation Count")


val_bplot


# Genral records

total_records <- val_explore |> n()
total_validations <- val_explore |> sum(NB_VALD)
average_validations <- val_explore |> mean(NB_VALD)
std_dev <- val_explore |> sd(NB_VALD)
min_val <- val_explore |> min(NB_VALD)
max_val <- val_explore |> max(NB_VALD)



ggplot()


val_zd[val_zd]
