library(tidyverse)
library(lme4)
library(lmerTest)
library(emmeans)
library(rstatix)
library(ggplot2)
library(modelsummary)
source("R/Load_Helpers.R")

### Look into sig words using un-adjusted VSOA.
## Don't use centered variable on x-axis.
## 255 words that differ. 
## Same sign, significant, different sign not significant. 
## Load most recent data
cdi_counts <- readRDS("data/cdi-metadata.rds") %>%
  count(lexical_class) %>%
  rename(n_class = n)
d <- load_most_recent_by_mtime("data","VSOA_Conc") %>%
  mutate(group = droplevels(group),
         group = as.factor(group)) %>%
  group_by(word, group) %>%
  mutate(sig = ifelse(sign(ci_l) == sign(ci_u), TRUE, FALSE), .after = ci_u) %>%
  filter(sig,group == "ASD-NA")

## Number of sig. differences
sum(d$sig)

## Lexical class counts 
d %>%
  group_by(lexical_class) %>%
  summarize(n = n())
## More iconic words tend to be learned later in ASD than NA
ggplot(d, aes(x = iconicity_rating, y = vsoa))+
  geom_point()+
  geom_smooth(method = "lm")+
  theme_bw()+
  annotate("text", label = "Learned Earlier in NA Group", x = 3.5, y = 300)+
  annotate("text", label = "Learned Earlier in ASD Group", x = 3.5, y = -300)+
  labs(y = "\u394 VSOA  (ASD-NA)",
       x = "Iconicity Rating")


ggplot(d %>% filter(!lexical_class %in% c("function_words",NA)), aes(x = iconicity_rating_centered, y = vsoa, color = lexical_class))+
  geom_point()+
  geom_smooth(method = "lm")+
  facet_wrap(~lexical_class)+
  labs(y = "\u394 VSOA  (ASD-NA)",
       x = "Iconicity Rating")+
  theme_bw()


## Not significant
ggplot(d, aes(x = imageability_rating, y = vsoa))+
  geom_point()+
  geom_smooth(method = "lm")+
  theme_bw()+
  annotate("text", label = "Learned Earlier in NA Group", x = 4, y = 300)+
  annotate("text", label = "Learned Earlier in ASD Group", x = 4, y = -300)+
  labs(y = "\u394 VSOA  (ASD-NA)",
       x = "Imageability Rating")


ggplot(d %>% filter(!lexical_class %in% c("function_words",NA)), aes(x = imageability_rating_centered, y = vsoa, color = lexical_class))+
  geom_point()+
  geom_smooth(method = "lm")+
  facet_wrap(~lexical_class)+
  labs(y = "\u394 VSOA  (ASD-NA)",
       x = "Imageability Rating")+
  theme_bw()

## Model with just imageability and Iconicity
m <- lm(vsoa ~ imageability_rating_centered + iconicity_rating_centered, data = d)
summary(m)
m2 <- lm(vsoa ~ imageability_rating_centered * iconicity_rating_centered, data = d)
summary(m2)
anova(m,m2)


## Plot same panel
d_plot <- d %>%
  select(word, lexical_class, Imageability = imageability_rating,Iconicity = iconicity_rating, vsoa) %>%
  pivot_longer(cols = starts_with("i"),
               names_to = "measure",
               values_to = "value") %>%
  filter(vsoa<500)
## Overall
ggplot(d_plot,aes(x = value, y = vsoa))+
  geom_point()+
  geom_smooth(method = "lm")+
  facet_wrap(~measure)+
  theme_bw()+
  annotate("text", label = "Learned Earlier in NA Group", x = 3.5, y = 300)+
  annotate("text", label = "Learned Earlier in ASD Group", x = 3.5, y = -300)+
  labs(y = "\u394 VSOA  (ASD-NA)",
       x = "Rating")
## By lexical class
ggplot(d_plot %>% filter(!lexical_class %in% c(NA)),aes(x = value, y = vsoa, color = lexical_class))+
  geom_point()+
  geom_smooth(method = "lm", se = FALSE)+
  facet_wrap(~measure)+
  theme_bw()+
  annotate("text", label = "Learned Earlier in NA Group", x = 3.5, y = 300)+
  annotate("text", label = "Learned Earlier in ASD Group", x = 3.5, y = -300)+
  labs(y = "\u394 VSOA  (ASD-NA)",
       x = "Rating")

ggplot(d_plot %>% filter(!lexical_class %in% c(NA)),aes(x = value, y = vsoa, color = lexical_class))+
  geom_point()+
  geom_smooth(method = "lm", se = FALSE)+
  facet_grid(lexical_class~measure)+
  theme_bw()+
  
  labs(y = "\u394 VSOA  (ASD-NA)",
       x = "Rating")

load_most_recent_by_mtime("data","VSOA_Conc") %>%
  mutate(group = droplevels(group),
         group = as.factor(group)) %>%
  group_by(word, group) %>%
  mutate(sig = ifelse(sign(ci_l) == sign(ci_u), TRUE, FALSE), .after = ci_u) %>%
  filter(word %in% d$word, !group == "ASD-NA", vsoa<500) %>%
  mutate(VSOA_cut = cut(vsoa, c(-256, -50,0, 50,200))) %>%
  group_by(group, VSOA_cut) %>%
  summarize(m = mean(iconicity_rating, na.rm = T),
            sd = sd(iconicity_rating, na.rm = T)) %>%
  as.data.frame



## Just lexical class
d_lex <- d %>%
  mutate(lexical_class = as.factor(lexical_class))

m_class <- lm(vsoa ~ lexical_class - 1, data = d_lex) 
summary(m_class)

plot_class <- data.frame(lexical_class = unique(m_class$model$lexical_class),
                         marginals = m_class$coefficients,
                         se = summary(m_class)$coefficients[,'Std. Error'])

ggplot(plot_class, aes(x = lexical_class,y = marginals, fill = lexical_class))+
  geom_col()+
  geom_errorbar(aes(ymin = marginals - se, ymax = marginals + se), width = 0.2)+
  theme_bw()+
  theme(legend.position = "none")+
  labs(y = "\u394 VSOA  (ASD-NA)",
       x = "Class")+
  annotate("text", label = "Learned Earlier in NA Group", x = 3, y = 50)+
  annotate("text", label = "Learned Earlier in ASD Group", x = 2, y = -75)


d |>
  mutate(earlier_ASD = ifelse(vsoa < 0, TRUE,FALSE)) |>
  filter(earlier_ASD) |>
  left_join(cdi_counts) |>
  group_by(lexical_class) |>
  summarize(n = n(),
            tot = n_class,
            perc = n/tot) |>
  unique()
  
  


