library(tidyverse); library(sf)
# which occur in SE region
rcs_index<-read.csv('../National-RCS/rcs_results/RCS_table_20201028.csv') %>%
  dplyr::select(species, RCS_WS, RCS_buff)

all_anuran_occ<- read.csv('../National-RCS/data/anuran_occ_all20200708.csv')
nrow(all_anuran_occ)
head(all_anuran_occ)
#count of all obs
anuran_count<-all_anuran_occ %>% count(final.taxa) %>%
  rename(total.obs='n')

crs.geo <- st_crs("+proj=longlat +ellps=WGS84 +datum=WGS84")
all_anuran_sf<-st_as_sf(x=all_anuran_occ, 
                        coords=c("Longitude","Latitude"), 
                        crs=crs.geo)

#modified SE USGS region
#losing IA, MO, TX & OK but gaining WV & VA

library(ggplot2); library(maps)
states<-st_as_sf(map('state', fill=T))
st_crs(states)==crs.geo

state_anurans<-st_join(all_anuran_sf, states)

SE_states<-c('mississippi', 'georgia',
             'florida','south carolina', 'north carolina', 'virginia',
              'alabama', 'tennessee')

(sp_choice_list<-state_anurans %>% 
  rename(state='ID.y') %>% filter(state == "virginia") %>%
  count(final.taxa) %>% 
  as_tibble() %>% dplyr::select(-geometry) %>%
  left_join(anuran_count, by='final.taxa') %>%
  mutate(percent.obs=(n/total.obs)*100) %>%
  arrange(desc(percent.obs)) %>%
  left_join(rcs_index, by=c('final.taxa'='species')) %>%
    mutate(select_group=case_when(RCS_WS <0.575 ~ 'not vulnerable',
                                  RCS_WS >=0.575 & RCS_WS < 0.75 ~ 'moderate',
                                  RCS_WS >=0.75 & RCS_WS < 0.825 ~ 'vulnerable',
                                  RCS_WS >=0.825 & RCS_WS <=1 ~ 'very vulnerable')))
sp_choice_list %>% filter(select_group=='not vulnerable') 

ggplot()+geom_histogram(data=sp_choice_list, aes(x=RCS_WS, fill=select_group),
                        binwidth=0.05)
sp_choice_list %>% count(select_group)
set.seed(10403)
(sampled_spp<-sp_choice_list %>% 
  group_by(select_group) %>%
  sample_n(4) %>%
  arrange(desc(RCS_WS)))


#AHH there is still 58 taxa
state_anurans %>% 
  rename(state='ID.y') %>% filter(state == 'virginia') %>%
  count(final.taxa)
#in virginia alone there is 34 taxa

#chosen subset
mnm_subset<-c('Lithobates capito', 'Pseudacris brimleyi', 
              'Pseudacris ornata', 'Lithobates grylio',
              'Pseudacris fouquettei', 'Pseudacris ocularis',
              'Pseudacris brachyphona', 'Anaxyrus quercicus',
              'Lithobates areolatus', 'Scaphiopus holbrookii',
              'Anaxyrus punctatus', 'Pseudacris triseriata',
              'Pseudacris nigrita', 'Lithobates catesbeianus',
              'Dryophytes versicolor', 'Anaxyrus americanus')
View(sp_choice_list %>% filter(final.taxa %in% mnm_subset) %>%
  arrange(RCS_WS))

ggplot()+
  geom_sf(data=states)+
  geom_sf(data=state_anurans[state_anurans$final.taxa=='Dryophytes versicolor',])
ggplot()+
  geom_sf(data=states)+
  geom_sf(data=state_anurans[state_anurans$final.taxa=='Anaxyrus americanus',])
ggplot()+
  geom_sf(data=states)+
  geom_sf(data=all_anuran_sf[all_anuran_sf$final.taxa=='Anaxyrus californicus',])


sp_choice_list$final.taxa


sp_lists_jan<-state_anurans %>% 
  rename(state='ID.y') %>% filter(state %in% SE_states) %>%
  group_by(final.taxa) %>% slice(1) %>%
  select(family, genus, final.taxa, state)
pdf('se_anuran_list.pdf')
for(i in sp_lists_jan$final.taxa){
  print(state_anurans %>% filter(final.taxa==i) %>%
    ggplot()+geom_sf(data=states)+geom_sf()+ggtitle(i))
}
dev.off()
