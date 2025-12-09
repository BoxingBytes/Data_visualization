library(readr)
library(dplyr)
library(ggplot2)
library(lubridate )
library(sf)
library(arrow)




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
