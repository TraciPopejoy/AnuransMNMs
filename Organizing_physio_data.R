library(tidyverse);library(googlesheets4); library(googledrive)

# Bring in the results of our concentrated physiological search ----
sp_files<-drive_find(team_drive = "Anuran Physiological Trait Search", 
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

# Bring in miscellaneous traits collected ----
FB_sheet<-drive_find(team_drive = "Anuran Physiological Trait Search", 
                     pattern="Parameters")
misc_sheet<-drive_find(team_drive = "Anuran Physiological Trait Search", 
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
trait_db_raw<-all_trait_shts %>%
  filter(!is.na(Value)) %>%
  rename(Taxa=taxa) %>%
  mutate(type='concentrated') %>%
  bind_rows(FB_trait_data %>% mutate(type='haphazard'),
            misc_trait_data %>% rename(Taxa=Species_orig) %>% mutate(type='haphazard')) %>%
  left_join(taxa_key) %>%
  mutate(Trait=recode(Trait, "Standard Metabolic Rate"="Metabolic Rate",
                      "Cutaneous Water Loss Rate"="Water Loss Rate")) %>%
  select(Taxa, species, Trait, everything()) 
write_csv(trait_db_raw, 'raw_data/combined_trait_data.csv')

trait_db_raw %>%
  arrange(type) %>%
  #remove traits that have been duplicated on accident
  distinct(species, Trait, `Life Stage`, 
           Sex, Value, Units, Location, Acclimation, .keep_all=T) %>%
  left_join(refs, by=c(`Source file name`="Source.file.name")) %>%
  write_csv('denormalized_trait_db.csv')

trait_db_raw %>%
  select(-Taxa, -`Search Terms Used`, -canonicalName, -doi, -first_author, -year)
