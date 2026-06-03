library(dplyr)
library(readr)
library(tidyverse)

## Load data
d_VSOA <- read_rds("data/vsoa-autistic-nonautistic-ndar-id-fix-remodel-v2.rds")
ezra_imag <- readxl::read_xlsx("data/Imageability_VSOA_SRCLD_may_analyses.xlsx")
iconicity_conc <- read.csv("data/pone.0137147.s001.csv")
concrete <- readxl::read_xlsx("data/13428_2013_403_MOESM1_ESM.xlsx")

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
         )

## Concreteness table
conc_meta <- iconicity_conc %>%
  select(word, concreteness_perry = concreteness) %>%
  unique() %>%
  full_join(select(concrete, word = Word, concreteness_brysbaert = Conc.M))

## Iconicity table
icon_meta <- iconicity_conc %>%
  filter(task == "written") %>%
  group_by(word) %>%
  summarize(average_iconicity_written = mean(rating)) %>%
  select(word, average_iconicity_written) %>%
  arrange(word)

## Join with JCPP VSOA data
d_joined <- d_VSOA %>%
  left_join(select(d_meta, num_item_id,lexical_class,aoa_produces, CHILDES_Freq, imageability_rating)) %>%
  unique() %>%
  left_join(icon_meta) %>%
  left_join(conc_meta) %>%
  mutate(concreteness_all = ifelse(!is.na(concreteness_perry) & !is.na(concreteness_brysbaert), concreteness_brysbaert,
                                   ifelse(is.na(concreteness_perry),concreteness_brysbaert,
                                          ifelse(is.na(concreteness_brysbaert),concreteness_perry,NA)))) %>%
  mutate(across(.cols = c(imageability_rating,average_iconicity_written, concreteness_all, concreteness_perry, concreteness_brysbaert),
                .fns = ~mean(.x, na.rm = T)-.x,
                .names = "{.col}_centered"))


saveRDS(d_joined, file = paste0("data/VSOA_Conc_Icon_Image_",Sys.Date(),".rds"))






