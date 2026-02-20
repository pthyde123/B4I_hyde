library(readr)
library(readxl)
library(tidyverse)

X2026_design_2026_layout_final_v3 <- read_csv("data/B4I_2026/2026_design_2026_layout_final_v3.csv")
Monoculture_oat_pea <- read_csv("data/B4I_2026/Monoculture_oat_pea.csv")
PM3D_randomization <- read_csv("data/B4I_2026/PM3D_randomization.csv")


X26NY_POI <- read_excel("data/B4I_2026/26NY_POI.xlsx")
X2026_iCrop_Seed_Shipping_Labels_v3 <- read_csv("data/B4I_2026/2026_iCrop_Seed-Shipping-Labels_v3.csv")



pea_ny <- X26NY_POI %>% 
  select (line_ID, name, `NY plots`, `g/plot` ) %>% 
  rename("peaE" = line_ID) %>% 
  rename("nPea_plots" = `NY plots`) %>% 
  rename(g_pea_per_plot = `g/plot`) %>% 
  rename(pea_id = name)



oat_ny <- X2026_iCrop_Seed_Shipping_Labels_v3 %>%
  
  filter(lLOC == "NY") %>% 
  mutate("oat_id" = lGID) %>%
  rename(oatE = lACCNO) %>% 
  mutate(g_oat_per_plot = 24) %>%
  rename(nOat_plots = NY) %>% 
  select(oat_id, oatE, nOat_plots, g_oat_per_plot)

 

randomization_ny <- X2026_design_2026_layout_final_v3 %>% 
  filter(location == "NY") %>% 
  select(location, oat_id, pea_id, block, row, col, plot_number) 





## Oat Bulk Packs 

NY_bulk_oat <- randomization_ny %>% 
  count(oat_id) %>% 
  
      left_join(oat_ny)  %>% 
      
      arrange(oatE)%>%
      mutate(g_per_oat_id = nOat_plots * g_oat_per_plot) %>% 
  
      print(n = 232)
 



## Pea Bulk Packs 
NY_bulk_pea <- randomization_ny %>% 
  count(pea_id) %>% 
  
  left_join(pea_ny)  %>% 
  
  arrange(nPea_plots,peaE)%>%
  mutate(g_per_pea_id = nPea_plots * g_pea_per_plot) %>% 
  
  print(n = 230)







  
  
  
## Oat planting packs

X2026_design_2026_layout_final_v3 %>% 
  filter(location == "NY") %>% 
  
      left_join(X2026_iCrop_Seed_Shipping_Labels_v3 %>%
              select(lGID,lLOC,lLOCwg,pLABEL01,pLABEL02,pLABEL03,lACCNO) %>% 
              filter(lLOC == "NY") %>% 
              mutate("oat_id" = lGID)) %>% 
  
   select(location, oat_id, pea_id, block, row, col, plot_number,lACCNO) 

  
  
  
  



