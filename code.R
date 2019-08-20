

# Packages and data -------------------------------------------------------

library(tidyverse)

setwd("~/Git/2019sle_ordinals/")

ord <- read_tsv("data/ord.csv")



# Regular counts ----------------------------------------------------------

table(ord$exp)

unexp <- ord %>%
  filter(exp == "unexp")

unique(unexp$stimulus_nr)
unique(unexp$sp_code)


# Confidence intervals ----------------------------------------------------


count <- ord %>%
  group_by(stimulus_nr, var) %>% 
  summarise(total = n())

# (omdat ik dit niet normaal automatisch kan doen)
write.csv(count, "count.csv")


# Load data for confidence intervals

ord_ci <- read_tsv("data/for_ci.csv")

ord_ci <- ord_ci[complete.cases(ord_ci$var),]

tibble(q_id = ord_ci$stimulus_nr,
       v_id = ord_ci$var_nr,
       v_value = ord_ci$var,
       expected = ord_ci$exp,
       n_observed = ord_ci$total,
       n_total = ord_ci$n) %>% 
  rowwise() %>% 
  mutate(mean_v = n_observed/n_total,
         ci_l = binom.test(n_observed, n_total)$conf.int[1],
         ci_u = binom.test(n_observed, n_total)$conf.int[2]) %>% 
  ggplot(aes(v_value, mean_v, color = expected, ymin = ci_l, ymax = ci_u))+
  geom_pointrange()+
  facet_wrap(~q_id, scales = "free_y")+
  coord_flip()+
  theme_bw()+
  labs(x = "", y = "")


# Only expected answer to questions 1, 2, 7, 10, 14, 16

# Most variation in 6, 8, 9, 13?

vari <- ord_ci %>%
  filter(stimulus_nr %in% c("3", "4", "5", "6", "8", "9", "11", "12", "13", "15", "17"))

tibble(q_id = vari$stimulus_nr,
       v_id = vari$var_nr,
       v_value = vari$var,
       expected = vari$exp,
       n_observed = vari$total,
       n_total = vari$n) %>% 
  rowwise() %>% 
  mutate(mean_v = n_observed/n_total,
         ci_l = binom.test(n_observed, n_total)$conf.int[1],
         ci_u = binom.test(n_observed, n_total)$conf.int[2]) %>%
  ggplot(aes(v_value, mean_v, color = expected, ymin = ci_l, ymax = ci_u))+
  geom_pointrange()+
  facet_wrap(~q_id, scales = "free_y")+
  coord_flip()+
  theme_bw()+
  labs(x = "", y = "")
  


