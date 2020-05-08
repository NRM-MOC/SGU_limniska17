library(tidyverse)
library(readxl)
library(MoCiS2) # devtools::install_github("NRM-MOC/MoCiS2")

kodlista_path <- system.file("extdata", "codelist.xlsx", package = "MoCiS2")
prc_data <- moc_read_prc("data/LIMN17prc.txt") # Reads a copy of the 2017-data in the limnic prc-file 

labs <- tribble(
  ~LAB, ~LABB,
  "ITMCB", "ACES CLC/BFR, Stockholms universitet",
  "ITMM", "ACES metall, Stockholms universitet",
  "ITMP", "ACES PFAS, Stockholms universitet",
  "UMK", "Umeå universitet, Kemiska institutionen",
  "UMU", "Umeå universitet, Kemiska institutionen",
  "NRM", "Naturhistoriska Riksmuseet"
)


SGU <- prc_data %>% 
  left_join(read_excel(kodlista_path, sheet = "STATIONER") %>% select(LOC, PROVPLATS_ID, NAMN_PROVPLATS)) %>% 
  left_join(read_excel(kodlista_path, sheet = "ARTER") %>% select(GENUS, ART, DYNTAXA_TAXON_ID)) %>% 
  left_join(read_excel(kodlista_path, sheet = "PARAMETRAR")  %>%
              select(NRM_PARAMETERKOD, PARAMETERNAMN, UNIK_PARAMETERKOD, ENHET, MATOSAKERHET_ENHET, PROV_LAGR),
            by = c("NRM_CODE" = "NRM_PARAMETERKOD")) %>% 
  left_join(labs) %>% 
  filter(!is.na(PARAMETERNAMN)) %>% 
  rename(PROV_KOD_ORIGINAL = ACCNR,
         ANTAL = NHOM,
         ) %>% 
  mutate(ANTAL_DAGAR = NA,
         KON = case_when(SEX == 1 ~ "M",
                         SEX == 2 ~ "F",
                         (SEX > 1) & (SEX < 2) ~ "X"),
         is_LOQ = (VALUE < 0) & !(NRM_CODE %in% c("D13CUCD", "D15NUCD")),
         MATVARDETAL_ANM = ifelse(is_LOQ, "<", ""),
         MATV_STD = ifelse(is_LOQ, "q", ""),
         MATVARDETAL = ifelse(is_LOQ, abs(VALUE), VALUE),
         RAPPORTERINGSGRANS_LOQ = ifelse(is_LOQ, MATVARDETAL, NA),
         UTFOR_LABB = ifelse(NRM_CODE %in% c("D13CUCD", "D15NUCD", "CUCD", "NUCD"), "UC Davies Stable isotope facility", ""),
         MATOSAKERHET = NA
         )

SGU <- left_join(SGU, read_csv("lab_details.csv"), by = c("PARAMETERNAMN", "LABB")) %>% # Lab details taken from limnic 2018 
  mutate(ANTAL_DAGAR = 1)


moc_write_SGU(SGU, "DATA_MATVARDE", "limniska17_DATA_MATVARDE.xlsx", program = "limn")
moc_write_SGU(SGU, "PROVDATA_BIOTA", "limniska17_PROVDATA_BIOTA.xlsx", program = "limn")
moc_write_SGU(SGU, "PROVMETADATA", "limniska17_PROVMETADATA.xlsx", program = "limn")
  