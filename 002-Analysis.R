library(tidyverse)
library(lme4)
library(lmerTest)
library(emmeans)
library(rstatix)
library(ggplot2)
library(modelsummary)
source("R/Load_Helpers.R")

## Load most recent data
d <- load_most_recent_by_mtime("data","VSOA_Conc") %>%
  mutate(vsoa_adjusted = ifelse(vsoa > 680, 680, vsoa), .after = vsoa) %>%
  filter(!group == "ASD-NA") %>%
  mutate(group = droplevels(group),
         group = as.factor(group))

contrasts(d$group) <- c(-0.5,0.5)
## Linear mixed model VSOA ~ group + imageability + concreteness + iconicity + group*iconicity + group*concreteness + group*imageability

d %>%
  group_by(group) %>%
  summarize(mean_VSOA = mean(vsoa_adjusted),
            sd_VSOA = sd(vsoa_adjusted)) %>%
  as.data.frame()

m <- lmer(vsoa_adjusted ~ group+CHILDES_Freq+
                 imageability_rating_centered+
                 iconicity_rating_centered+
                 group*imageability_rating_centered+
                 group*iconicity_rating_centered+
            (1 | word),
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
summary(lm(vsoa~iconicity_rating_centered+CHILDES_Freq, data = d, subset = group == "ASD"))
summary(lm(vsoa~iconicity_rating_centered+CHILDES_Freq, data = d, subset = group == "NA"))

## Imageability simple
summary(lm(vsoa~imageability_rating, data = d, subset = group == "ASD"))
summary(lm(vsoa~imageability_rating, data = d, subset = group == "NA"))

d_plot <- d %>%
  pivot_longer(cols = ends_with("centered"),
               names_to = "measure",
               values_to = "value")

ggplot(d_plot %>% filter(!group == "ASD-NA",
                         !measure %in% c("concreteness_perry_centered",
                                         "concreteness_brysbaert_centered",
                                         "concreteness_all_centered")), aes(x = value, y = vsoa, color = group))+
 # geom_point(alpha = 0.4)+
  geom_smooth(method = "lm")+
  facet_wrap(~measure,
             scales = "free",nrow = 2,ncol = 1,
             labeller = as_labeller(c("imageability_rating_centered" = "Imageability",
                                      "iconicity_rating_centered" = "Iconicity")))+
  theme_bw()
ggsave("Figures/VSOA_wordfeat_byGroup.png",
       dpi = 300)




## Add how much data we have for each word in the IVs

d_numbs <- d %>% 
  select(word,lexical_class, iconicity_rating,imageability_rating) %>%
  pivot_longer(cols = c(iconicity_rating,imageability_rating),
               names_to = "measure",
               values_to = "value") %>%
  unique() 

## Overall
d_numbs %>%
  group_by(measure) %>%
  summarize(n_NA = sum(is.na(value)),
            n_nonNA = sum(!is.na(value)),
            prop_NA = sum(is.na(value)/n()),
            n = n())

## By class
d_numbs %>%
  group_by(measure, lexical_class) %>%
  summarize(n_NA = sum(is.na(value)),
            n_nonNA = sum(!is.na(value)),
            prop_NA = sum(is.na(value)/n()),
            n = n())


wrdlvl_nums <- d_numbs %>%
  group_by(word) %>%
  mutate(is.complete = ifelse(sum(!is.na(value)) == 2, TRUE,FALSE)) %>%
  ungroup()

wrdlvl_nums %>%
  select(word,is.complete) %>%
  unique() %>%
  summarize(sum(is.complete))



## Correlation between concreteness and imageability, iconcicity
cor(d %>% 
       select(word, concreteness_all, iconicity_rating,imageability_rating) %>%
      unique %>%
  select(-word), use = "pairwise.complete.obs", method = 'pearson') 



## Nested models for imageability, imageability and concreteness. Or
## Remove one and see if the model converges. 

m <- lmer(vsoa_adjusted ~ group+
            imageability_rating_centered+
            iconicity_rating+
            group*imageability_rating_centered+
            group*iconicity_rating+
            (1 | word) + (1 | group),
          control = lmerControl(optimizer = "Nelder_Mead"),
          data = d %>% filter(!group == "ASD-NA"))
summary(m)


## Nouns #####
m_nouns <- lmer(vsoa_adjusted ~ group+CHILDES_Freq+
            imageability_rating_centered+
            iconicity_rating_centered+
            group*imageability_rating_centered+
            group*iconicity_rating_centered+
            (1 | word),
          data = d %>% filter(!group == "ASD-NA", lexical_class == "nouns"))
summary(m_nouns)

modelsummary(
  list("Model" = m_nouns),
  output = "data/modelsummary_nouns.docx",
  stars = T,
  title = "LMEM Results Nouns",
  statistic = c("std.error","statistic","p.value"),
  shape = term~model+statistic
)

ggplot(d_plot %>% filter(!group == "ASD-NA",
                         lexical_class == "nouns",
                         !measure %in% c("concreteness_perry_centered",
                                         "concreteness_brysbaert_centered",
                                         "concreteness_all_centered")), aes(x = value, y = vsoa, color = group))+
  #geom_point(alpha = 0.4)+
  geom_smooth(method = "lm")+
  facet_wrap(~measure,
             scales = "free",nrow = 3,ncol = 1,
             labeller = as_labeller(c("imageability_rating_centered" = "Imageability",
                                      "iconicity_rating_centered" = "Iconicity")))+
  theme_bw()
ggsave("Figures/VSOA_wordfeat_byGroupNouns.png",
       dpi = 300)



## Verbs #####
m_verbs <- lmer(vsoa_adjusted ~ group+CHILDES_Freq+
                  imageability_rating_centered+
                  iconicity_rating_centered+
                  group*imageability_rating_centered+
                  group*iconicity_rating_centered+
                  (1 | word),
                data = d %>% filter(!group == "ASD-NA", lexical_class == "verbs"))
summary(m_verbs)

modelsummary(
  list("Model" = m_verbs),
  output = "data/modelsummary_verbs.docx",
  stars = T,
  title = "LMEM Results Verbs",
  statistic = c("std.error","statistic","p.value"),
  shape = term~model+statistic
)

ggplot(d_plot %>% filter(!group == "ASD-NA",
                         lexical_class == "verbs",
                         !measure %in% c("concreteness_perry_centered",
                                         "concreteness_brysbaert_centered",
                                         "concreteness_all_centered")), aes(x = value, y = vsoa, color = group))+
  #geom_point(alpha = 0.4)+
  geom_smooth(method = "lm")+
  facet_wrap(~measure,
             scales = "free",nrow = 3,ncol = 1,
             labeller = as_labeller(c("imageability_rating_centered" = "Imageability",
                                      "iconicity_rating_centered" = "Iconicity")))+
  theme_bw()

## Adjectives ######
m_adj <- lmer(vsoa_adjusted ~ group+CHILDES_Freq+
                  imageability_rating_centered+
                  iconicity_rating_centered+
                  group*imageability_rating_centered+
                  group*iconicity_rating_centered+
                  (1 | word),
                data = d %>% filter(!group == "ASD-NA", lexical_class == "adjectives"))
summary(m_adj)


modelsummary(
  list("Model" = m_adj),
  output = "data/modelsummary_adj.docx",
  stars = T,
  title = "LMEM Results Adjective",
  statistic = c("std.error","statistic","p.value"),
  shape = term~model+statistic
)

ggplot(d_plot %>% filter(!group == "ASD-NA",
                         lexical_class == "adjectives",
                         !measure %in% c("concreteness_perry_centered",
                                         "concreteness_brysbaert_centered",
                                         "concreteness_all_centered")), aes(x = value, y = vsoa, color = group))+
  #geom_point(alpha = 0.4)+
  geom_smooth(method = "lm")+
  facet_wrap(~measure,
             scales = "free",nrow = 3,ncol = 1,
             labeller = as_labeller(c("imageability_rating_centered" = "Imageability",
                                      "iconicity_rating_centered" = "Iconicity")))+
  theme_bw()

### Interaction
m_image_adj_NA <- lm(vsoa_adjusted ~ imageability_rating_centered+CHILDES_Freq, 
                     data = d,
                     subset = c(group == "NA", lexical_class == "adjectives")) 
summary(m_image_adj_NA)

m_image_adj_ASD <- lm(vsoa_adjusted ~ imageability_rating_centered+CHILDES_Freq, 
                     data = d,
                     subset = c(group == "ASD", lexical_class == "adjectives")) 
summary(m_image_adj_ASD)

m_icon_adj_NA <- lm(vsoa_adjusted ~ iconicity_rating_centered+CHILDES_Freq, 
                     data = d,
                     subset = c(group == "NA", lexical_class == "adjectives")) 
summary(m_icon_adj_NA)

m_icon_adj_ASD <- lm(vsoa_adjusted ~ iconicity_rating_centered+CHILDES_Freq, 
                      data = d,
                      subset = c(group == "ASD", lexical_class == "adjectives")) 
summary(m_icon_adj_ASD)


## Other #####
m_other <- lmer(vsoa_adjusted ~ group+CHILDES_Freq+
                imageability_rating_centered+
                iconicity_rating_centered+
                group*imageability_rating_centered+
                group*iconicity_rating_centered+
                (1 | word),
              data = d %>% filter(!group == "ASD-NA", lexical_class == "other"))
summary(m_other)


modelsummary(
  list("Model" = m_other),
  output = "data/modelsummary_other.docx",
  stars = T,
  title = "LMEM Results Other",
  statistic = c("std.error","statistic","p.value"),
  shape = term~model+statistic
)


ggplot(d_plot %>% filter(!group == "ASD-NA",
                         lexical_class == "other",
                         !measure %in% c("concreteness_perry_centered",
                                         "concreteness_brysbaert_centered",
                                         "concreteness_all_centered")), aes(x = value, y = vsoa, color = group))+
  #geom_point(alpha = 0.4)+
  geom_smooth(method = "lm")+
  facet_wrap(~measure,
             scales = "free",nrow = 3,ncol = 1,
             labeller = as_labeller(c("imageability_rating_centered" = "Imageability",
                                      "iconicity_rating_centered" = "Iconicity")))+
  theme_bw()


### Interaction
m_icon_other_NA <- lm(vsoa_adjusted ~ iconicity_rating_centered+CHILDES_Freq, 
                     data = d %>% filter(lexical_class == "other"),
                     subset = c(group == "NA")) 
summary(m_icon_other_NA)

m_icon_other_ASD <- lm(vsoa_adjusted ~ iconicity_rating_centered+CHILDES_Freq, 
                      data = d %>% filter(lexical_class == "other"),
                      subset = c(group == "ASD")) 
summary(m_icon_other_ASD)
