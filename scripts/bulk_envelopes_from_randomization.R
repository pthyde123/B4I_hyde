


library(readr)
library(tidyverse)


pea_oat_experiment_design <- read_csv("data/pea_oat_experiment_design.csv")


pea_oat_experiment_design %>% 
  select(Pea,Location,PlotNumber) %>% 
  group_by(Pea,Location) %>% 
  count() %>% 
  arrange(desc(n)) %>% 
  mutate(g_plot = 60)



