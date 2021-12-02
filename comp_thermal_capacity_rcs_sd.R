#check that sd climate change matches what we know of anuran physiology
library(tidyverse)
rcs.cs<-read.csv('../National-RCS/rcs_results/Anuran RCS values 20210405.csv') %>%
  select(species, WS_CS, CS_WS_adj, buff_CS)
raw_tem_sd<-read.csv('../National-RCS/rcs_results/intermediate results/taxa_ws_climate_values20210402.csv') %>%
  filter(!grepl('ppt',value_type),
         value_origin=='huc12') %>%
  group_by(species, value_origin, value_type) %>%
  summarize(WS_sd=sd(value))
trait_db_raw<-read_csv('denormalized_trait_db.csv')
sd_vs_therm<-trait_db_raw %>%
  filter(Trait %in% c('CTmax','CTmin'),
         `Life Stage`=='adult') %>%
  select(species, Trait, Value) %>%
  group_by(species, Trait) %>%
  summarize(v=mean(as.numeric(Value), na.rm=T)) %>%
  pivot_wider(names_from=Trait, values_from=v) %>%
  filter(!is.na(CTmin)&!is.na(CTmax)) %>%
  mutate(thermal_capacity=CTmax-CTmin) %>%
  left_join(raw_tem_sd ) %>%
  filter(!is.na(value_type)) 
ggplot(sd_vs_therm,aes(x=thermal_capacity, y=WS_sd))+
  geom_point()+geom_smooth(method='lm')+
  facet_wrap(~value_type, scales='free')
summary(lm(WS_sd~thermal_capacity, data=sd_vs_therm[sd_vs_therm$value_type=='tmax',]))
summary(lm(WS_sd~thermal_capacity, data=sd_vs_therm[sd_vs_therm$value_type=='tmin',]))

        