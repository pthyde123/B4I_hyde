library(readr)
library(readxl)
library(tidyverse)

### First section formats the trials, this is the same as for the packing lables
### import the trials, there are three, main, mono, PM3D
X2026_design_2026_layout_final_v3 <- read_csv("data/B4I_2026/2026_design_2026_layout_final_v3.csv")
Monoculture_oat_pea <- read_csv("data/B4I_2026/Monoculture_oat_pea.csv")
PM3D_randomization <- read_csv("data/B4I_2026/PM3D_randomization.csv")

### import the seed packing metadata on each acceccion
X26NY_POI <- read_excel("data/B4I_2026/26NY_POI.xlsx")
X2026_iCrop_Seed_Shipping_Labels_v3 <- read_csv("data/B4I_2026/2026_iCrop_Seed-Shipping-Labels_v3.csv")


### format the ny pea meta 
pea_ny <- X26NY_POI %>% 
  select (line_ID, name, `NY plots`, `g/plot` ) %>% 
  rename("peaE" = line_ID) %>% 
  rename("nPea_plots" = `NY plots`) %>% 
  rename(g_pea_per_plot = `g/plot`) %>% 
  rename(pea_id = name)


### format the ny oat meta
oat_ny <- X2026_iCrop_Seed_Shipping_Labels_v3 %>%
  
  filter(lLOC == "NY") %>% 
  mutate("oat_id" = lGID) %>%
  rename(oatE = lACCNO) %>% 
  mutate(g_oat_per_plot = 24) %>%
  rename(nOat_plots = NY) %>% 
  select(oat_id, oatE, nOat_plots, g_oat_per_plot)


### filter the trials to just NY location
randomization_ny <- X2026_design_2026_layout_final_v3 %>% 
  filter(location == "NY") %>% 
  select(location, oat_id, pea_id, block, row, col, plot_number) 

mono_ny <- Monoculture_oat_pea %>% 
  filter(environment == "NY")

PM3D_ny <- PM3D_randomization %>% 
  filter(environment == "NY")

#########################################################################
########This is the good part, combining 3 trials into one###############

### adding in the mono and pm3d trial to the main randomization  

runif(1)*10000
set.seed(6708) # set seed once 6708

full_randomization <- set.seed(6708) %>%
  
  bind_rows(
    
    randomization_ny %>% 
      mutate(trial = "B4I_main") %>% 
      mutate(rand_plot = plot_number) %>% 
      mutate(plot_type = "combo") %>% 
      select(location, trial, plot_number, oat_id, pea_id, rand_plot, plot_type),
    
    mono_ny %>%  
      mutate(rand_plot = sort(round(runif(n=20, min=1, max=400), 2) )) %>% 
      rename(location = environment)%>% 
      select(location, trial, plot_number,  oat_id, pea_id, rand_plot, plot_type),
    
    PM3D_ny %>%  
      mutate(rand_plot = sort(round(runif(n=20, min=1, max=400), 2) )) %>% 
      rename(location = environment)%>% 
      select(location, trial, plot_number,  oat_id, pea_id, rand_plot, plot_type)
  ) %>% 
  
  arrange(rand_plot) %>% 
  mutate(plot_seq = seq(1:440))%>% 
  
  mutate(row = rep(c(1:22), each = 20)) %>% 
  mutate(col = rep(c(1:20, 20:1), times = 11))


full_randomization

ggplot(full_randomization, aes(x = col, y = row, fill = plot_number)) +
  geom_tile()

unique(pea_ny$pea_id)

### Field Map 

map <- full_randomization %>% 
  mutate(plot = str_c(plot_number,"_",str_sub(trial,1,1))) %>% 
  select(plot,row,col) %>% 
  pivot_wider(names_from = col,
              values_from = plot) %>% 
  arrange(-row)



write.table(map, "clipboard", sep="\t", row.names=FALSE)

### Save randomization ###

#write.csv(full_randomization, "data/B4I_2026/2026_B4I_NY_randomization.csv", row.names = F)


