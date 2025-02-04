
library(readxl)
library(tidyverse)

#oat needed
AL_replacement_seed_needed <- read_excel("data/AL replacement seed needed.xlsx")
X2025_iCrop_Oat_Seed_Shipment_labels <- read_excel("data/2025_iCrop_Oat-Seed-Shipment_labels.xlsx") %>% 
  select(gid,sn)

oat_needs <- AL_replacement_seed_needed %>% 
  filter(crop == "oat") %>% 
  left_join(X2025_iCrop_Oat_Seed_Shipment_labels, by = join_by("Accession"=="gid"))

#pea needed
library(readr)
peaEntry_accessionPea <- read_csv("data/peaEntry-accessionPea.csv") %>% 
  select(peaEntry,NAME)

pea_needs <- AL_replacement_seed_needed %>% 
  filter(crop == "pea") %>% 
  left_join(peaEntry_accessionPea, by = join_by("Accession"=="NAME"))

# oat and pea needed

bind_rows(oat_needs,pea_needs)

write.csv(bind_rows(oat_needs,pea_needs),"data/AL_replacement_seed.csv",row.names = F)


