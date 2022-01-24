library(tidyverse)

trait_db_raw<-read_csv('denormalized_trait_db_22.csv') %>%
  filter(!(Trait %in% c("Water Loss Rate",
                        "Minimum Egg Development Temperature",
                        "Maximum Egg Development Temperature",
                        "Metabolic Rate")))
refs <- read_csv('ATraiU 2.0/Reference_List.csv')
taxa_key<-read.csv('ATraiU 2.0/taxonomic_key.csv')
foc_taxa<-gsub(' MNM traits','', sp_files$name)

# GlobTherm comparison ----
globaltherm<-read.csv('C:/Users/Owner/Downloads/GlobTherm/GlobalTherm_upload_02_11_17.csv')
gt_anurans<-globaltherm %>% filter(Order=='Anura') %>%
  mutate(species1=paste(Genus, Species)) %>%
  filter(species1 %in% c(trait_db_raw$Taxa, trait_db_raw$species))

names(gt_anurans)
gt_anurans %>% select(Genus, Species, N, Tmax, tmin, REF_max, REF_min) %>% View()
# references missed
# Christian_et_al_1988 --- E. coqui (not searched for)
# MacArthur_&_Dandy_1982 - should have picked up
library(readxl)
globaltherm_ref<-readxl::read_xlsx('C:/Users/Owner/Downloads/GlobTherm/References_1_09_2017.xlsx')

globaltherm_ref %>% filter(grepl('Christian_et_al_1988', `Data citation`) | 
                             grepl('MacArthur_&_', `Data citation`)) %>% 
  pull(Reference)

trait_db_raw %>%
  filter(type == 'concentrated' & Trait %in% c('CTmax', 'CTmin') & `Life Stage`=='adult',
         !(`Source file name` %in% c('Brattstrom 1968', 'Layne1985_CTmin',
                                     'Miller & Packard 1977'))) %>%
  View()

# AmphiBIO comparison ---- 
amphibio<-read.csv('C:/Users/Owner/Downloads/AmphiBIO_v1/AmphiBIO_v1.csv')
amphibioCOMP<-amphibio %>% filter(Order =='Anura') %>% 
  # select only columns shared between datasets
  dplyr::select(Family, Genus, Species, Noc, Diu, Crepu, Body_mass_g, OBS) %>% 
  # join the taxa key to make species align
  right_join(taxa_key, by=c("Species"="Taxa"))%>%
  filter(species %in% foc_taxa,
         !is.na(Genus)) %>%
  # join with our trait database
  right_join(trait_db_raw %>% 
               # isolate columns we need & pivot to wide DB
              filter(Trait %in% c('Activity','Mass'), 
                     `Life Stage` %in% c('adult','unknown')) %>%
              select(species, Trait, Value) %>%
              group_by(species) %>% 
               # each row will be a species, with
               # all the mass values listed recorded for that sp
               pivot_wider(names_from=Trait, values_from=Value, values_fn=list)) %>%
  rowwise() %>%
  # sum Activities to see which lacks all values (==0)
  mutate(amb_act=sum(Noc, Diu, Crepu, na.rm=T)) %>%
  # only concerned with our focal taxa
  filter(species %in% foc_taxa) %>%
  select(species, Body_mass_g, Mass, Activity, amb_act, Noc, Diu, Crepu, everything()) 
View(amphibioCOMP)
amphibioCOMP %>% ungroup() %>%
  mutate(a=case_when(is.na(Body_mass_g) & Mass=='NULL' ~'unknown',
                     !is.na(Body_mass_g) & Mass =='NULL' ~'amp',
                     is.na(Body_mass_g) & !is.na(Mass)~'atraiu',
                     T ~ 'both')) %>%
  filter(species %in% foc_taxa) %>% 
  group_by(a) %>% View()

amphibioCOMP %>% ungroup() %>%
  mutate(a=case_when(is.na(Body_mass_g) & Mass=='NULL' ~'unknown',
                     !is.na(Body_mass_g) & Mass =='NULL' ~'amp',
                     is.na(Body_mass_g) & !is.na(Mass)~'atraiu',
                     T ~ 'both')) %>%
  filter(species %in% foc_taxa) %>% 
  group_by(a) %>% tally()

amphibioCOMP %>% 
  filter(amb_act == 0) %>% 
  group_by(species) %>% slice(1) %>% nrow()

View(amphibio)
