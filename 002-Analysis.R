library(tidyverse)
library(lme4)
library(lmerTest)
library(emmeans)
library(ggplot2)
source("R/Load_Helpers.R")

## Load most recent data
d <- load_most_recent_by_mtime("data","VSOA_Conc")


## Linear mixed model VSOA ~ group + imageability + concreteness + iconicity + group*iconicity + group*concreteness + group*imageability

m <- lmer(vsoa ~ group+
                 imageability_rating_centered+
                 concreteness_all_centered+
                 average_iconicity_written_centered+
                 group*imageability_rating_centered+
                 group*concreteness_all_centered+
                 group*average_iconicity_written_centered+
            (1 | word) + (1 | group),
          control = lmerControl(optimizer = "Nelder_Mead"),
          data = d %>% filter(!group == "ASD-NA"))
summary(m)


d_plot <- d %>%
  pivot_longer(cols = ends_with("centered"),
               names_to = "measure",
               values_to = "value")
em <- emmeans(m,specs = c("group",
                 "imageability_rating_centered",
                 "concreteness_all_centered",
                 "average_iconicity_written_centered"))

ggplot(d_plot %>% filter(!group == "ASD-NA",
                         !measure %in% c("concreteness_perry_centered",
                                         "concreteness_brysbaert_centered")), aes(x = value, y = vsoa, color = group))+
  geom_point()+
  geom_smooth(method = "lm")+
  facet_wrap(~measure,
             scales = "free",
             labeller = as_labeller(c("imageability_rating_centered" = "Imageability",
                                      "concreteness_all_centered" = "Concreteness",
                                      "average_iconicity_written_centered" = "Iconicity")))+
  theme_bw()
