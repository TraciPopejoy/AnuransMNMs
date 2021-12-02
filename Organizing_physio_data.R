library(tidyverse);library(googlesheets4); library(googledrive)

# Bring in the results of our concentrated physiological search ----
sp_files<-drive_find(shared_drive = "Anuran Physiological Trait Search", 
           pattern="MNM traits")
1
# number of species in concentrated search
nrow(sp_files)
all_trait_shts<-NULL
for(i in 1:nrow(sp_files)){
  file.id<-sp_files[[2]][i]
  file.taxa<-sub(" MNM traits", "", sp_files[[1]][i])
  sht<-read_sheet(file.id, col_types = 'c') %>% 
    mutate(taxa=file.taxa)
  all_trait_shts<-bind_rows(all_trait_shts,sht)
}
1

write_csv(all_trait_shts, paste0('raw_data/RAW_concentrated_trait_values',
                                 format(Sys.Date(), "%Y%m%d"),'.csv'))

#replace N/A and n/a with NA
all_trait_shts <- all_trait_shts %>%
  mutate(across(everything()), na_if(.,"N/A")) %>%
  mutate(across(everything()), na_if(.,"n/a"))

#remove traits we are not interested in, but were collected before
#and rows that have no value associated with them
unique(all_trait_shts$Trait)[-c(7,11:16)]
all_trait_shts_mod<-all_trait_shts %>%
  filter(Trait %in% unique(all_trait_shts$Trait)[-c(8,11:16)],
         !is.na(Value),
         `Source file name` != 'Animal Diversity Web') 
View(all_trait_shts_mod)

# Bring in miscellaneous traits collected ----
FB_sheet<-drive_find(shared_drive = "Anuran Physiological Trait Search", 
                     pattern="Parameters")
misc_sheet<-drive_find(shared_drive = "Anuran Physiological Trait Search", 
           pattern="Misc")
FB_trait_data<-read_sheet(FB_sheet$id, sheet="Table 9.2", col_types='c') %>%
   mutate(Trait='Tpref', Units='C') %>%
  bind_rows(read_sheet(FB_sheet$id, sheet="Ch8Text", col_types='c'))
View(FB_trait_data)

misc_trait_data<-read_sheet(misc_sheet$id, col_types='c')

write_csv(FB_trait_data, paste0('raw_data/RAW_FB_trait_values',
                                 format(Sys.Date(), "%Y%m%d"),'.csv'))
write_csv(misc_trait_data, paste0('raw_data/RAW_misc_trait_values',
                                 format(Sys.Date(), "%Y%m%d"),'.csv'))

library(rgbif)
anura_tax<-unique(c(misc_trait_data$Species_orig, FB_trait_data$Taxa, all_trait_shts$taxa))
#known bad species
#"Anaxyrus valliceps" "Rana palustris"     "Hyla saufferi"      "Hyla sp."           "Hypopachus sp."    
#"Rana hedscheri"     "Hyla raddiana"  
anuran.gbif.taxa<-NULL
for(u in anura_tax){
  anuran.gbif.taxa1<-name_backbone(name=u, 
                                   rank='species', kingdom='animals')
  anuran.gbif.taxa<-bind_rows(anuran.gbif.taxa1 %>% mutate(Taxa=u), 
                              anuran.gbif.taxa)
}
View(anuran.gbif.taxa)
# TODO check taxa for spelling mistakes
taxa_key<- anuran.gbif.taxa %>%
  select(Taxa, canonicalName, family, genus, species)

# Build Reference List ----
# identify ones with doi and cross reference
library(rcrossref)
doi.refs<-all_trait_shts %>% 
  filter(!is.na(doi),
         !(doi %in% c("NULL", "-", 28)),
         !grepl('Rausch', `Source file name`)) %>%
  group_by(doi) %>% slice(1) %>% ungroup() %>%
  select(`Source file name`,doi) %>% 
  rowwise() %>% 
  mutate(citation=list(cr_cn(dois=sub("DOI ", "", doi), format='text')))
#will get lots of blue text, just need to hand fix Rausch
#49 is bad
doi.refs<-doi.refs %>%
  mutate(citation=ifelse(is.null(citation),NA,unlist(citation)))

# bring it all back together into one reference list
refs_list<-all_trait_shts %>%
  select(taxa, `Source file name`, doi) %>%
  filter(!is.na(`Source file name`)) %>%
  group_by(`Source file name`) %>% slice(1) %>%
  left_join(doi.refs) 
  
write_csv(refs_list, paste0('raw_data/RAW_references_', format(Sys.Date(), "%Y%m%d"), '.csv'))
sum(is.na(refs_list$CITE))
# looked up the rest of the references by hand
refs<-read.csv('References_20210817.csv') %>% 
  group_by(citation) %>% 
  mutate(group_id=cur_group_id()) %>%
  ungroup()
View(refs)

write_csv(refs, 'ATraiU 2.0/Reference_List.csv')

# Final Collation ----
#long data base - taxa, trait, value, source name, source number, other accompanying info
#references - year, citation, source name, source number
# Combine all trait information
trait_db_raw<-all_trait_shts_mod %>%
  rename(Taxa=taxa) %>%
  mutate(type='concentrated') %>%
  bind_rows(FB_trait_data %>% mutate(type='haphazard'),
            misc_trait_data %>% rename(Taxa=Species_orig) %>% mutate(type='haphazard')) %>%
  left_join(taxa_key) %>%
  # clean up the text
  mutate(Trait=recode(Trait, "Standard Metabolic Rate"="Metabolic Rate",
                      "Cutaneous Water Loss Rate"="Water Loss Rate"),
         `Life Stage`=recode(`Life Stage`, 'juveniles' = 'juvenile', 
                             'unkown'='unknown', 'tapdole'='tadpole')) %>%
  select(Taxa, species, Trait, everything())  
write_csv(trait_db_raw, 'raw_data/combined_trait_data.csv')

refs<-read.csv('ATraiU 2.0/Reference_List.csv')

trait_db_raw %>%
  arrange(type) %>%
  #remove traits that have been duplicated on accident
  distinct(species, Trait, `Life Stage`, 
           Sex, Value, Units, Location, Acclimation, .keep_all=T) %>%
  left_join(refs, by=c(`Source file name`="Source.file.name")) %>%
  write_csv('denormalized_trait_db.csv')

trait_long<-trait_db_raw %>%
  left_join(refs, by=c(`Source file name`="Source.file.name")) %>% 
  select(-Taxa, -`Search Terms Used`, -DOI, -Notes, -page,-order,-Season,
         -rechecked,-doi, -citation,-journal,-issue, -year,-second_author,
         -canonicalName,  -ends_with('author')) %>%
  rename(RefID=group_id)

# species x trait summary matrix 
#need to seperate character traits and convert to binary matrix (n records?)
# then cbind with averaged? other traits
unique(trait_long$Trait)
#character traits
tchar<-trait_long %>%
  group_by(species, `Life Stage`, Trait) %>%
  filter(Trait == 'Activity') %>%
  mutate(record=1,
         activity=tolower(recode(Value, 
                                 'nocturnal and diurnal'='diurnal, nocturnal')))%>%
  ungroup() %>%
  dplyr::select(family, genus, species, RefID, `Source file name`, activity, record) %>%
  pivot_wider(names_from=activity, values_from=record) %>%
  mutate(nocturnal=case_when(!is.na(`diurnal, nocturnal`)~ 1,
                             T~nocturnal),
         diurnal=case_when(!is.na(`diurnal, nocturnal`)~1,
                           T~diurnal)) %>%
  group_by(species) %>%
  select(-'diurnal, nocturnal') %>%
  summarize(RefIDs_a=paste(unique(RefID), collapse=', '),
            across('nocturnal':'arrhythmic', sum, na.rm=T)) %>%
  mutate(`Life Stage`='adult')

tbif<-trait_long %>%
  mutate(`Life Stage` = case_when(grepl('gosner', `Life Stage`) ~ 'tadpole',
                                  `Life Stage`=='unknown' ~ 'adult',
                                  T ~ `Life Stage`)) %>%
  group_by(species, `Life Stage`, Trait) %>%
  filter(!(Trait %in% c("Activity","Water Loss Rate",
                        "Minimum Egg Development Temperature",
                        "Maximum Egg Development Temperature",
                        "Metabolic Rate")),
         `Life Stage`=='adult') %>%
  mutate(v_n=as.numeric(Value)) %>%
  select(species, `Life Stage`, RefID, Trait, Value, v_n) %>%
  pivot_wider(names_from=Trait, values_from = v_n, values_fn=mean) %>%
  group_by(species, `Life Stage`) %>%
  summarize(RefIDs_n=paste(unique(RefID), collapse=', '),
            Mass=ifelse(!all(is.na(Mass)), mean(Mass,na.rm=T), NA),
            CTmax=ifelse(!all(is.na(CTmax)), max(CTmax, na.rm=T), NA),
            CTmin=ifelse(!all(is.na(CTmin)), min(CTmin, na.rm=T), NA), 
            Tpref=ifelse(!all(is.na(Tpref)), mean(Tpref, na.rm=T),  NA),
            Tbask=ifelse(!all(is.na(Tbask)), mean(Tbask, na.rm=T),  NA),
            Tforage_optim=ifelse(!all(is.na(Tforage_optim)), mean(Tforage_optim, na.rm=T), NA),
            Tforage_max=ifelse(!all(is.na(Tforage_max)), max(Tforage_max, na.rm=T), NA),  
            Tforage_min=ifelse(!all(is.na(Tforage_min)), min(Tforage_min, na.rm=T), NA), 
            Tmerge=ifelse(!all(is.na(Tmerge)), min(Tmerge, na.rm=T), NA))
# warnings are from Tmerge == overwinters
View(tbif)
  
trait_long %>% filter( species == "Lithobates sphenocephalus", grepl('gosner', `Life Stage`))
tbif$Units %>% unique()

full_join(tchar, tbif, by=c('species','Life Stage')) %>%
  mutate(RefIDs=paste(RefIDs_n, RefIDs_a, sep=', ')) %>%
  select(-RefIDs_a, -RefIDs_n, -`Life Stage`) %>%
  select(species, everything()) %>%
  write.csv('ATraiU2_summary_values_2021NOV.csv')

#Table 1----

trait_long %>%
  group_by(species, Trait) %>%
  filter(`Life Stage`=='adult',
         !is.na(Value)) %>%
  slice(1) %>%
  group_by(species) %>%  count() %>%
  right_join(anuran.gbif.taxa %>%
               filter(species %in% gsub(' MNM traits', '', sp_files$name)) %>%
               select(family, genus, species) %>%
               group_by(species) %>% slice(1)) %>%
  select(family, genus, species, n) %>%
  arrange(family) %>%
  write.csv('AT2_fig/table_1_211028.csv', row.names = F)
  
