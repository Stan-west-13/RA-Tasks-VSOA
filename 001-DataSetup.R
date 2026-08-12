library(dplyr)
library(readr)
library(tidyverse)

## Load data
d_VSOA <- read_rds("data/vsoa-autistic-nonautistic-ndar-id-fix-remodel-v2.rds")
ezra_imag <- readxl::read_xlsx("data/Imageability_VSOA_SRCLD_may_analyses.xlsx")
iconicity_conc <- read.csv("data/pone.0137147.s001.csv")
concrete <- readxl::read_xlsx("data/13428_2013_403_MOESM1_ESM.xlsx")
iconicity_new <- read.csv("data/iconicity_ratings_cleaned.csv") %>% 
  mutate(word = tolower(word)) 
## Select relevant columms from Ezra table
d_meta <- ezra_imag %>%
  select(num_item_id = `num_item_id...1`,
         word,
         lemma,
         category,
         lexical_class,
         aoa_produces,
         CHILDES_Freq,
         group = Group,
         adjusted.VSOA,
         imageability_rating,
         ) %>%
  mutate(lemma = sub("\\+.*", "", lemma))

## Concreteness table
conc_meta <- iconicity_conc %>%
  select(word, concreteness_perry = concreteness) %>%
  unique() %>%
  full_join(select(concrete, word = Word, concreteness_brysbaert = Conc.M))

## Iconicity table
icon_meta <- iconicity_new %>%
  select(word, iconicity_rating = rating) %>%
  arrange(word) %>%
  unique() 

## Join with JCPP VSOA data
## Only use winter.
d_joined <- d_VSOA %>%
  left_join(select(d_meta, lemma,num_item_id,lexical_class,aoa_produces, CHILDES_Freq, imageability_rating)) %>%
  unique() %>%
  left_join(conc_meta, by = c("lemma" = "word")) %>%
  left_join(icon_meta, by = c("lemma" = 'word')) %>%
  mutate(concreteness_all = ifelse(!is.na(concreteness_perry) & !is.na(concreteness_brysbaert), concreteness_brysbaert,
                                   ifelse(is.na(concreteness_perry),concreteness_brysbaert,
                                          ifelse(is.na(concreteness_brysbaert),concreteness_perry,NA)))) %>%
  mutate(across(.cols = c(imageability_rating, iconicity_rating, concreteness_all, concreteness_perry, concreteness_brysbaert),
                .fns = ~.x - mean(.x, na.rm = T),
                .names = "{.col}_centered")) %>%
  mutate(vsoa_adjusted = ifelse(vsoa < 0, 0 , 
                                ifelse(vsoa > 680, 680, vsoa)), .after = vsoa)

saveRDS(d_joined, file = paste0("data/VSOA_Conc_Icon_Image_",Sys.Date(),".rds"))
