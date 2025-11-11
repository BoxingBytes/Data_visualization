library(readr)
library(dplyr)
val_2025_first <- read_csv2("data/validations-reseau-ferre-nombre-validations-par-jour-1er-trimestre.csv")
val_2025_second <- read_csv2("data/validations-reseau-ferre-nombre-validations-par-jour-2eme-trimestre.csv")
val_2018_first <- read_delim("data/data-rf-2018/2018_S1_NB_FER.txt", delim = "\t")
val_2018_second <- read_delim("data/data-rf-2018/2018_S2_NB_FER.txt", delim = "\t")
val_2019_first <- read_delim("data/data-rf-2019/2019_S1_NB_FER.txt", delim = "\t")
val_2019_second <- read_delim("data/data-rf-2019/2019_S2_NB_FER.txt", delim = "\t")
val_2020_first <- read_delim("data/data-rf-2020/2020_S1_NB_FER.txt", delim = "\t")
val_2020_second <- read_delim("data/data-rf-2020/2020_S2_NB_FER.txt", delim = "\t")
val_2021_first <- read_delim("data/data-rf-2021/2021_S1_NB_FER.txt", delim = "\t")
val_2021_second <- read_delim("data/data-rf-2021/2021_S2_NB_FER.txt", delim = "\t")
val_2022_first <- read_delim("data/data-rf-2022/2022_S1_NB_FER.txt", delim = "\t")
val_2022_second <- read_delim("data/data-rf-2022/2022_S2_NB_FER.txt", delim = ";")
val_2023_first <- read_delim("data/data-rf-2023/2023_S1_NB_FER.txt", delim = "\t")
val_2023_second <- read_delim("data/data-rf-2023/2023_S2_NB_FER.txt", delim = "\t", locale = locale(encoding = "UTF-16"))
val_2024_first <- read_delim("data/data-rf-2024/2024_S1_NB_FER.txt", delim = "\t")

val_2025_second <- rename(val_2025_second, ID_REFA_LDA = ID_ZDC)
val_2025_first <- rename(val_2025_first, ID_REFA_LDA = ID_ZDC)
val_2023_second <- rename(val_2023_second, ID_REFA_LDA = ID_ZDC)
val_2023_first <- rename(val_2023_first, ID_REFA_LDA = lda)
val_2024_first <- rename(val_2024_first, ID_REFA_LDA = ID_ZDC)
val_2022_second <- rename(val_2022_second, ID_REFA_LDA = lda)

val_merged <- rbind(
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
# 6,726 more rows with NA but there are all empty so we just remove them 
# na_df <- val_merged[!complete.cases(val_merged),]
# write.csv(na_df, "data/val_merged_na.csv")

val_merged <- val_merged |> na.omit()

library(sf)
zd <- st_read("data/REF_ZdA/PL_ZDL_R_11-11-2025.shp")

zd_grouped <- zd %>%
  group_by(idrefa_lda) %>%
  summarise(geometry = st_union(geometry))

val_zd <- left_join(val_merged, zd_grouped, by = c("ID_REFA_LDA" = "idrefa_lda"))
st_write(val_zd,"val_zd.shp")

