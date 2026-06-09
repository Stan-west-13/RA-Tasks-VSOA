library(tidyverse)
library(lme4)
library(lmerTest)
library(emmeans)
library(rstatix)
library(ggplot2)
library(modelsummary)
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

modelsummary(
  list("Model" = m),
  output = "data/modelsummary.docx",
  stars = T,
  title = "LMEM Results",
  statistic = c("std.error","statistic","p.value"),
  shape = term~model+statistic,
)

## Concreteness simple
summary(lm(vsoa~concreteness_all_centered, data = d, subset = group == "ASD"))
summary(lm(vsoa~concreteness_all_centered, data = d, subset = group == "NA"))

## Iconicity simple
summary(lm(vsoa~average_iconicity_written_centered, data = d, subset = group == "ASD"))
summary(lm(vsoa~average_iconicity_written_centered, data = d, subset = group == "NA"))


d_plot <- d %>%
  pivot_longer(cols = ends_with("centered"),
               names_to = "measure",
               values_to = "value")

ggplot(d_plot %>% filter(!group == "ASD-NA",
                         !measure %in% c("concreteness_perry_centered",
                                         "concreteness_brysbaert_centered")), aes(x = value, y = vsoa, color = group))+
  geom_point(alpha = 0.4)+
  geom_smooth(method = "lm")+
  facet_wrap(~measure,
             scales = "free",nrow = 3,ncol = 1,
             labeller = as_labeller(c("imageability_rating_centered" = "Imageability",
                                      "concreteness_all_centered" = "Concreteness",
                                      "average_iconicity_written_centered" = "Iconicity")))+
  theme_bw()
ggsave("Figures/VSOA_wordfeat_byGroup.png",
       dpi = 300)

## LG transformed


d_lg10 <- d %>%
  mutate(across(.cols = c(concreteness_all,average_iconicity_written),
                .names = "{.col}_lg10",
                .fns = log10))

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

modelsummary(
  list("Model" = m),
  output = "data/modelsummary.docx",
  stars = T,
  title = "LMEM Results",
  statistic = c("std.error","statistic","p.value"),
  shape = term~model+statistic,
)