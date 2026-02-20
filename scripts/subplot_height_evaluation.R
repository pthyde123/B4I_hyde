
library(tidyverse)
library(readr)
B4I_2025_ALL_phenotypes <- read_csv("data/B4I_2025_ALL_phenotypes.csv")


colnames(B4I_2025_ALL_phenotypes)


data <- B4I_2025_ALL_phenotypes %>% 

  mutate(oat_yield = `Grain yield - g/m2|CO_350:0000260`) %>% 
  mutate(pea_yield = `Pea Grain Yield - g/m2|CO_xxx:0003008`) %>% 
  mutate( rust = `Crown rust severity (flag leaf) - percent|CO_350:0005029`) %>% 
  mutate(oat_height_2 = `Plant height - cm|timepoint 2|COMP:0000115`) %>% 
  mutate(pea_height_2 = `Pea Plant Height - cm|timepoint 2|COMP:0000123`) %>% 
  
  select(observationUnitName, studyName,germplasmName,intercropGermplasmName,rust, 
         oat_yield, pea_yield,oat_height_2, pea_height_2, observationLevel,locationName ) %>% 
  filter(observationUnitName  != "B4I_2025_NY_5265")


data




plot_oat_yield <- data %>% 
  filter(observationLevel == "plot") %>% 
  select(observationUnitName, oat_yield, locationName)



subplot_mean_height <- data %>% 
  filter(observationLevel == "subplot") %>% 
  mutate(sub_name = str_sub(observationUnitName,start = 0, end = 16)) %>% 
  select(sub_name,oat_height_2) %>% 
  group_by(sub_name) %>% 
  summarise(mean_oat_height_T2 = mean(oat_height_2))



subplot_2_height <- data %>% 
  filter(observationLevel == "subplot") %>% 
  
  mutate(sub_number = str_sub(observationUnitName,start = -1, end = -1)) %>% 
  mutate(sub_name = str_sub(observationUnitName,start = 0, end = 16)) %>% 
  
  filter(sub_number == 2) %>% 
  
  select(sub_name,sub_number,oat_height_2) %>% 
  rename(sub_2_oat_height_T2 = oat_height_2)
  
  


  
plot_oat_yield %>% 
  left_join(subplot_mean_height, join_by(observationUnitName == sub_name)) %>% 
  left_join(subplot_2_height, join_by(observationUnitName == sub_name)) %>% 
  
  filter(locationName != "Tallassee, AL") %>% 
  filter(locationName != "Urbana, IL") %>%

ggplot(aes(mean_oat_height_T2,oat_yield, color= locationName))+
  geom_point() +
  geom_smooth(se = TRUE, method = lm)+
  xlab("Oat Height")+
  ylab("Oat Yield")+
  scale_color_brewer(palette="Dark2")+
  theme_classic()+
  theme(axis.text.x=element_text(angle = 90, hjust = 1))+
  theme(axis.text=element_text(size=20), axis.title=element_text(size=20,face="bold"))+
  theme(axis.text.x = element_text(face = "bold", color = "black", size = 16))+
  theme(axis.text.y = element_text(face = "bold", color = "black", size = 16))




plot_oat_yield %>% 
  left_join(subplot_mean_height, join_by(observationUnitName == sub_name)) %>% 
  left_join(subplot_2_height, join_by(observationUnitName == sub_name)) %>% 
  
  filter(locationName != "Tallassee, AL") %>% 
  filter(locationName != "Urbana, IL") %>% 
  
  ggplot(aes(sub_2_oat_height_T2,oat_yield, color= locationName))+
  geom_point() +
  geom_smooth(se = TRUE, method = lm)+
  xlab("Oat Height")+
  ylab("Oat Yield")+
  scale_color_brewer(palette="Dark2")+
  theme_classic()+
  theme(axis.text.x=element_text(angle = 90, hjust = 1))+
  theme(axis.text=element_text(size=20), axis.title=element_text(size=20,face="bold"))+
  theme(axis.text.x = element_text(face = "bold", color = "black", size = 16))+
  theme(axis.text.y = element_text(face = "bold", color = "black", size = 16))




lm_data <- plot_oat_yield %>% 
  left_join(subplot_mean_height, join_by(observationUnitName == sub_name)) %>% 
  left_join(subplot_2_height, join_by(observationUnitName == sub_name)) %>% 
  
  filter(locationName != "Tallassee, AL") %>% 
  filter(locationName != "Urbana, IL") 


lm <- lm(oat_yield ~ mean_oat_height_T2 + locationName, lm_data ) 
model_summary <- summary(lm)
r_squared_value <- (model_summary$r.squared)
r_squared_value

lm <- lm(oat_yield ~ sub_2_oat_height_T2 + locationName, lm_data ) 
model_summary <- summary(lm)
r_squared_value <- (model_summary$r.squared)
r_squared_value





