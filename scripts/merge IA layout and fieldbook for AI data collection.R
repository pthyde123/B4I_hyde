### Set up of Field Book file for IA


library(readxl)
B4I_2025_IA_fieldbook_plot <- read_excel("data/B4I_2025_IA_fieldbook_plot.xls")

B4I_2025_IA_fieldbook_subplot <- read_excel("data/B4I_2025_IA_fieldbook_subplot.xls")

Pea_Oat_Field_Layout_IA <- read_excel("data/Pea-Oat Field Layout_IA.xlsx")

B4I_2025_trait_summary <- read_excel("data/B4I_2025_trait_summary.xlsx")



### plot_trait_headings

t3_plot_traits <- B4I_2025_trait_summary %>% 
  select(`T3/Oat Trait ID`,evaluation_level,likely_collection_order) %>% 
  arrange(likely_collection_order) %>% 
  filter(evaluation_level == "plot") %>% 
  rename(t3_plot_traits = `T3/Oat Trait ID`) %>% 
  select(t3_plot_traits)


plot_traits <- data.frame(matrix(ncol = 7, nrow = 400))
colnames(plot_traits) <- t3_plot_traits$t3_plot_traits

plot_traits


### subplot_trait_headings

t3_subplot_traits <- B4I_2025_trait_summary %>% 
  select(`T3/Oat Trait ID`,evaluation_level,likely_collection_order) %>% 
  arrange(likely_collection_order) %>% 
  filter(evaluation_level == "subplot") %>% 
  rename(t3_subplot_traits = `T3/Oat Trait ID`) %>% 
  select(t3_subplot_traits)


subplot_traits <- data.frame(matrix(ncol = 19, nrow = 1200))
colnames(subplot_traits) <- t3_subplot_traits$t3_subplot_traits

subplot_traits


## Plot_fieldbook

plot_fieldbook <- bind_cols(Pea_Oat_Field_Layout_IA %>% 
  mutate(plot_number = Entry+2000) %>% 
  left_join(B4I_2025_IA_fieldbook_plot),
  plot_traits)

plot_fieldbook

#write.csv(plot_fieldbook, "data/B4I_2025_IA_plot_collection.csv",row.names = F)

## subplot_fieldbook

Pea_Oat_Field_Layout_IA


fieldbook_subplot <- B4I_2025_IA_fieldbook_subplot %>% 
  mutate(plot_subplot = str_c(plot_number,"_",subplot_number))


IA_subplot <- Pea_Oat_Field_Layout_IA %>% slice(rep(row_number(), 3)) %>%
  mutate(plot = Entry+2000) %>% 
  mutate(subplot = (rep(1:3, time = 400))) %>% 
  
  mutate(plot_subplot = str_c(plot,"_",subplot)) %>% 
     
    left_join(fieldbook_subplot, by = "plot_subplot") %>% 
  select(-plot,-subplot)

subplot_fieldbook <- bind_cols(IA_subplot,subplot_traits)


subplot_fieldbook

#write.csv(subplot_fieldbook, "data/B4I_2025_IA_subplot_collection.csv",row.names = F)


########################################################



#### IA PM3D layout for data collection
library(tidyverse)
library(readr)
B4I_2025_PM3D_IA_layout <- read_csv("data/B4I_2025_PM3D_IA_layout.csv")

X2025_PM3D_biomass_timepoints <- read_csv("data/2025_PM3D_biomass_timepoints.csv")

library(readxl)
B4I_2025_trait_summary <- read_excel("data/B4I_2025_trait_summary.xlsx")

PM3D_subplot_traits <- B4I_2025_trait_summary %>% 
  select(`T3/Oat Trait ID`,evaluation_level,likely_collection_order) %>% 
  arrange(likely_collection_order) %>% 
  filter(evaluation_level == "subplot") %>% 
  rename(t3_subplot_traits = `T3/Oat Trait ID`) %>% 
  select(t3_subplot_traits) %>% 
  filter(str_detect(t3_subplot_traits, 'iomass'))


subplot_traits_biomass <- data.frame(matrix(ncol = 9, nrow = 60))
colnames(subplot_traits_biomass) <- PM3D_subplot_traits$t3_subplot_traits

subplot_traits_biomass




layout <- B4I_2025_PM3D_IA_layout %>% 
  left_join(X2025_PM3D_biomass_timepoints, by = join_by("subplot_name" == "observationUnitName")) %>% 
  select(subplot_name,accession_name,plot_number,subplot_number,block_number,row_number,col_number,biomass_timepoint,biomass_T1,biomass_T2,biomass_T3) %>% 
  mutate(plot_subplot = str_c(plot_number,"_",subplot_number)) %>% 
  relocate(plot_subplot)


PM3D_IA_fieldbook <- bind_cols(layout, subplot_traits_biomass)


PM3D_IA_fieldbook

####write.csv(PM3D_IA_fieldbook, "data/B4I_2025_IA_PM3D_collection.csv",row.names = F)
