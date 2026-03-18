library(readr)
library(readxl)
library(tidyverse)


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
  


### Oat Bulk Packs 
# these are arranged by number of plots(better for seed divider) then oat entry number(best for sorting out envelopes)
# print these on sticky labels, probably don't need entries with 1 plot, they can go strait to planting envelopes.

NY_bulk_oat <- randomization_ny %>% 
  count(oat_id) %>% 
  
      left_join(oat_ny)  %>% 
      
      arrange(n,oatE)%>%
      mutate(g_per_oat_id = nOat_plots * g_oat_per_plot) %>% 
  
      print(n = 232)
 



### Pea Bulk Packs 
# these are arranged by number of plots(better for seed divider) then pea entry number(best for sorting out envelopes)
# print these on sticky labels, probably don't need entries with 1 plot, they can go strait to planting envelopes.
NY_bulk_pea <- randomization_ny %>% 
  count(pea_id) %>% 
  
  left_join(pea_ny)  %>% 
  
  arrange(nPea_plots,peaE)%>%
  mutate(g_per_pea_id = nPea_plots * g_pea_per_plot) %>% 
  
  print(n = 230)


  
## Oat planting packs
# use csv and envelope printer to print these

oat_envelopes <- randomization_ny %>%
  left_join(oat_ny, join_by(oat_id)) %>% 
  left_join(pea_ny, join_by(pea_id)) %>% 
  
  select(plot_number, oat_id, oatE, pea_id, peaE, g_oat_per_plot, g_pea_per_plot, nOat_plots, nPea_plots) %>% 

  arrange(nOat_plots,oatE) %>% 
  mutate(source = str_c("oatE ",oatE, "   plots ",nOat_plots)) %>% 
  mutate(peaName = str_c(pea_id, " peaE ", peaE)) %>%
  
  rename("PlotNo" = plot_number) %>% 
  rename(oatName = oat_id) %>% 
  
  select(source,PlotNo,oatName,peaName)


oat_envelopes
  
  
## Pea planting packs  
# use csv and envelope printer to print these

pea_envelopes  <- 
  randomization_ny %>%
  left_join(oat_ny, join_by(oat_id)) %>% 
  left_join(pea_ny, join_by(pea_id)) %>% 
  
  select(plot_number, oat_id, oatE, pea_id, peaE, g_oat_per_plot, g_pea_per_plot, nOat_plots, nPea_plots) %>% 
  
  arrange(nPea_plots,peaE) %>% 
  mutate(source = str_c("peaE ",peaE, "   plots ",nPea_plots)) %>% 
  mutate(peaName = str_c(pea_id, " peaE ", peaE)) %>%
  
  rename("PlotNo" = plot_number) %>% 
  rename(oatName = oat_id) %>% 
  
  select(source,PlotNo,oatName,peaName)

pea_envelopes

### adding in the mono and pm3d trial to the main randomization  
## envelopes for mono and pm3d can be made separately and added into the correct spot in the planting box. 

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


### Mono trial and PM3D trial envelopes

extra_oat_envelopes <-
full_randomization %>% 
  filter(trial %in% c("PM3D", "Monoculture")) %>%
  mutate(
    pea_id = recode(
      pea_id,
      "Amariilo"   = "CDC AMARILLO",
      "ND Victory" = "ND VICTORY",
      "Greenwood"  = "GREENWOOD",
      "CDC Inca"   = "CDC INCA",
      "Arcadia"    = "ARCADIA"
    )
  ) %>% 
 
  left_join(oat_ny, join_by(oat_id)) %>% ## nOat_plots refers to the main trial. 
  left_join(pea_ny, join_by(pea_id)) %>% 
 
  select(trial, plot_number, oat_id, pea_id, rand_plot, oatE, g_oat_per_plot, peaE, g_pea_per_plot,plot_type) %>% 
  
  arrange(oat_id) %>% 
  mutate(source = plot_type) %>% 
  mutate(peaName = pea_id) %>%
  
  mutate(trial_abv = recode(
    trial,
    "PM3D"   = "PM3D",
    "Monoculture" = "mono"
  )) %>% 
  
  mutate(plot_number = str_c (plot_number,"-", trial_abv,  " aka ", rand_plot)) %>% 
  
  rename("PlotNo" = plot_number) %>% 
  rename(oatName = oat_id) %>% 
  
  select(source,PlotNo,oatName,peaName)



####pea envelopes  

extra_pea_envelopes <- full_randomization %>% 
  filter(trial %in% c("PM3D", "Monoculture")) %>%
  mutate(
    pea_id = recode(
      pea_id,
      "Amariilo"   = "CDC AMARILLO",
      "ND Victory" = "ND VICTORY",
      "Greenwood"  = "GREENWOOD",
      "CDC Inca"   = "CDC INCA",
      "Arcadia"    = "ARCADIA"
    )
  ) %>% 
  
  left_join(oat_ny, join_by(oat_id)) %>% ## nOat_plots refers to the main trial. 
  left_join(pea_ny, join_by(pea_id)) %>% 
  
  select(trial, plot_number, oat_id, pea_id, rand_plot, oatE, g_oat_per_plot, peaE, g_pea_per_plot) %>% 
  
  mutate(trial_abv = recode(
    trial,
    "PM3D"   = "PM3D",
    "Monoculture" = "mono"
  )) %>% 
  
  arrange(peaE) %>% 
  mutate(source = str_c("peaE ",peaE)) %>% 
  mutate(oatName = oat_id) %>%
  
  mutate(plot_number = str_c (plot_number,"-", trial_abv,  " aka ", rand_plot)) %>%
  
  rename("PlotNo" = plot_number) %>% 
  rename(peaName = pea_id) %>% 
  
  mutate(g_pea_per_plot = if_else(is.na(oatName), round(g_pea_per_plot*1.666667,1
                                                        ), g_pea_per_plot)) %>% 
  
  mutate(peaName = str_c(peaName,"  ", g_pea_per_plot,"g_per_plot")) %>% 
  
  select(source,PlotNo,oatName,peaName)



### saving files
#write.csv(NY_bulk_oat, "data/B4I_2026/packing_labels/2026_B4I_bulk_oat_labels.csv", row.names = F)
#write.csv(NY_bulk_pea, "data/B4I_2026/packing_labels/2026_B4I_bulk_pea_labels.csv", row.names = F)

#write.csv(oat_envelopes, "data/B4I_2026/packing_labels/2026_B4I_planting_oat_labels.csv", row.names = F)
#write.csv(pea_envelopes, "data/B4I_2026/packing_labels/2026_B4I_planting_pea_labels.csv", row.names = F)


#write.csv(extra_pea_envelopes, "data/B4I_2026/packing_labels/2026_B4I_mono_pm3d_pea_labels.csv", row.names = F)
#write.csv(extra_oat_envelopes, "data/B4I_2026/packing_labels/2026_B4I_mono_pm3d_oat_labels.csv", row.names = F)




########

full_randomization %>% 
  filter(trial %in% c("PM3D", "Monoculture")) %>%
  mutate(
    pea_id = recode(
      pea_id,
      "Amariilo"   = "CDC AMARILLO",
      "ND Victory" = "ND VICTORY",
      "Greenwood"  = "GREENWOOD",
      "CDC Inca"   = "CDC INCA",
      "Arcadia"    = "ARCADIA"
    )
  ) %>% 
  
  left_join(oat_ny, join_by(oat_id)) %>% ## nOat_plots refers to the main trial. 
  left_join(pea_ny, join_by(pea_id)) %>% 
  
  select(trial, plot_number, oat_id, pea_id, rand_plot, oatE, g_oat_per_plot, peaE, g_pea_per_plot,plot_type)






#### Extra 
# gut check, counting entries not used in NY
total_oat_entries <- X2026_design_2026_layout_final_v3 %>% 
  count(oat_id)

NY_oat_entries  <- X2026_design_2026_layout_final_v3 %>% 
    filter(location == "NY")  %>%  
    count(oat_id)
  
nrow(total_oat_entries) - nrow(NY_oat_entries)
