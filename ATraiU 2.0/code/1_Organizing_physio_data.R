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

write_csv(all_trait_shts, paste0('ATraiU 2.0/intermediate_results/RAW_concentrated_trait_values',
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
#View(all_trait_shts_mod)

# Bring in miscellaneous traits collected ----
FB_sheet<-drive_find(shared_drive = "Anuran Physiological Trait Search", 
                     pattern="Parameters")
misc_sheet<-drive_find(shared_drive = "Anuran Physiological Trait Search", 
           pattern="Misc")
FB_trait_data<-read_sheet(FB_sheet$id, sheet="Table 9.2", col_types='c') %>%
   mutate(Trait='Tpref', Units='C') %>%
  bind_rows(read_sheet(FB_sheet$id, sheet="Ch8Text", col_types='c'))
#View(FB_trait_data)

misc_trait_data<-read_sheet(misc_sheet$id, col_types='c')

write_csv(FB_trait_data, paste0('ATraiU 2.0/intermediate_results/RAW_FB_trait_values',
                                 format(Sys.Date(), "%Y%m%d"),'.csv'))
write_csv(misc_trait_data, paste0('ATraiU 2.0/intermediate_results/RAW_misc_trait_values',
                                 format(Sys.Date(), "%Y%m%d"),'.csv'))

library(rgbif)
anura_tax<-unique(c(misc_trait_data$Species_orig, FB_trait_data$Taxa, all_trait_shts$taxa))
anuran.gbif.taxa<-NULL
for(u in anura_tax){
  anuran.gbif.taxa1<-name_backbone(name=u, 
                                   rank='species', kingdom='animals')
  anuran.gbif.taxa<-bind_rows(anuran.gbif.taxa1 %>% mutate(dataScientificName=u), 
                              anuran.gbif.taxa)
}
View(anuran.gbif.taxa)
# checked taxa for spelling mistakes on 1/20
taxa_key_start<- anuran.gbif.taxa %>%
  select(dataScientificName, canonicalName, scientificName, family, genus, species)
write.csv(taxa_key_start, 'ATraiU 2.0/intermediate_results/taxonomic_key_start.csv', row.names = F)
# edited table to link stray taxa to new names using ITIS, i.e. Hyla raddiana
name_backbone(name='Boana riojana', rank='species', kingdom = 'animals') %>%
  select(canonicalName, scientificName, family, genus, species)
taxa_key <- read.csv('ATraiU 2.0/ATraiU2_taxonomic_key_2022JAN.csv')
taxa_key_start[!(taxa_key_start$dataScientificName %in% taxa_key$dataScientificName),]

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
refs<-read.csv('ATraiU 2.0/intermediate_results/References_20210817.csv') %>% 
  group_by(citation) %>% 
  mutate(group_id=cur_group_id()) %>%
  rename(refID=group_id) %>%
  ungroup()
head(refs)

write_csv(refs, 'ATraiU 2.0/ATraiU2_reference_list_2022JAN.csv')

# Collate Data ----
# Combine all trait information
trait_db_raw<-all_trait_shts_mod %>%
  rename(dataScientificName=taxa) %>%
  mutate(dataCollationType=ifelse(dataScientificName == 'Pseudacris maculata',
                                  'haphazard', 'concentrated')) %>%
  bind_rows(FB_trait_data %>% 
              rename(dataScientificName=Taxa) %>% 
              mutate(dataCollationType='haphazard'),
            misc_trait_data %>% 
              rename(dataScientificName=Species_orig) %>% 
              mutate(dataCollationType='haphazard')) %>%
  left_join(taxa_key) %>%
  # clean up the text
  mutate(Trait=recode(Trait, "Standard Metabolic Rate"="Metabolic Rate",
                      "Cutaneous Water Loss Rate"="Water Loss Rate"),
         `Life Stage`=recode(`Life Stage`, 'juveniles' = 'juvenile', 
                             'unkown'='unknown', 'tapdole'='tadpole')) %>%
  select(dataScientificName, species, Trait, everything())  
write_csv(trait_db_raw, 'ATraiU 2.0/intermediate_results/combined_trait_data.csv')

# check the chorus trilling frog clade & Acris ----
trait_db_raw %>%
  filter(genus == 'Pseudacris',
         !grepl('Hyla ', dataScientificName)) %>%  View()
trait_db_raw %>%
  filter(genus == 'Acris') %>%
  select(dataScientificName, species, Location, 
         Trait, `Source file name`) %>%  View()

# create a denormalized trait database of all the data ----
refs<-read.csv('ATraiU 2.0/ATraiU2_reference_list_2022JAN.csv') #bring the reference data in
trait_db_raw %>%
  arrange(dataCollationType) %>%
  #remove traits that have been duplicated on accident
  distinct(species, Trait, `Life Stage`, 
           Sex, Value, Units, Location, Acclimation, .keep_all=T) %>%
  left_join(refs, by=c(`Source file name`="Source.file.name")) %>% 
  rename(traitName = Trait, traitValue = Value, traitUnit = Units,
          lifeStage=`Life Stage`, sex=Sex, verbatimLocality=Location) %>%
  write_csv('ATraiU 2.0/intermediate_results/denormalized_trait_db.csv')

# Final Collation -----
# long dataframe, each row a unique trait value
trait_long<-trait_db_raw %>%
  left_join(refs, by=c(`Source file name`="Source.file.name")) %>% 
  # remove columns that are superfluous for larger tables
  select(-dataScientificName, 
         -`Search Terms Used`, -DOI, -Notes, -page,-order,-Season,
         -doi, -citation,-journal,-issue, -year,-second_author,
         -ends_with('author'), -Acclimation, -`Original Source`) %>%
  # rename columns to match ecological trait standards when possible
  rename(traitName = Trait, traitValue = Value, traitUnit = Units,
         lifeStage=`Life Stage`, sex=Sex, verbatimLocality=Location,
         # these just clarify & match style of above standards
         traitVariation=`Variation of Value`,
         traitVariationType = `Measure of Variation`,
         traitSource=`Source file name`, 
         traitEvidenceCategory=`Strength of Evidence`) %>%
  ungroup() %>%
  # clean up sex column
  mutate(sex=ifelse(is.na(sex), 'unknown',sex))
trait_long$traitVariationType %>% unique()
trait_long %>% filter(is.na(refID)) %>% View()

trait_long<-trait_long %>% 
  filter(!is.na(refID)) #removing traits from websites

# species x trait summary matrix 
#need to seperate character traits and convert to matrix of n records
# then cbind with summarized, numeric other traits
unique(trait_long$traitName)
#character traits
tchar<-trait_long %>%
  group_by(species, lifeStage, traitName) %>%
  filter(traitName == 'Activity') %>%
  mutate(record=1,
         activity=tolower(recode(traitValue, 
                                 'nocturnal and diurnal'='diurnal, nocturnal')))%>%
  ungroup() %>%
  dplyr::select(family, genus, species, refID, traitSource, activity, record) %>%
  pivot_wider(names_from=activity, values_from=record) %>%
  mutate(nocturnal=case_when(!is.na(`diurnal, nocturnal`)~ 1,
                             T~nocturnal),
         diurnal=case_when(!is.na(`diurnal, nocturnal`)~1,
                           T~diurnal)) %>%
  group_by(species) %>%
  select(-'diurnal, nocturnal') %>%
  summarize(refIDs_a=paste(unique(refID), collapse=', '),
            across('nocturnal':'arrhythmic', sum, na.rm=T)) %>%
  mutate(lifeStage='adult')

tbif<-trait_long %>%
  mutate(lifeStage = case_when(grepl('gosner', lifeStage) ~ 'tadpole',
                                  lifeStage=='unknown' ~ 'adult',
                                  T ~ lifeStage)) %>%
  group_by(species, lifeStage, traitName) %>%
  filter(!(traitName %in% c("Activity","Water Loss Rate",
                        "Minimum Egg Development Temperature",
                        "Maximum Egg Development Temperature",
                        "Metabolic Rate")),
         lifeStage=='adult') %>%
  mutate(v_n=as.numeric(traitValue)) %>%
  select(species, lifeStage, refID, traitName, traitValue, v_n) %>%
  pivot_wider(names_from=traitName, values_from = v_n, values_fn=mean) %>%
  group_by(species, lifeStage) %>%
  summarize(refIDs_n=paste(unique(refID), collapse=', '),
            Mass=ifelse(!all(is.na(Mass)), mean(Mass,na.rm=T), NA),
            CTmax=ifelse(!all(is.na(CTmax)), max(CTmax, na.rm=T), NA),
            CTmin=ifelse(!all(is.na(CTmin)), min(CTmin, na.rm=T), NA), 
            Tpref=ifelse(!all(is.na(Tpref)), mean(Tpref, na.rm=T),  NA),
            Tbask=ifelse(!all(is.na(Tbask)), mean(Tbask, na.rm=T),  NA),
            Tforage_optim=ifelse(!all(is.na(Tforage_optim)), mean(Tforage_optim, na.rm=T), NA),
            Tforage_max=ifelse(!all(is.na(Tforage_max)), max(Tforage_max, na.rm=T), NA),  
            Tforage_min=ifelse(!all(is.na(Tforage_min)), min(Tforage_min, na.rm=T), NA), 
            Tmerge=ifelse(!all(is.na(Tmerge)), min(Tmerge, na.rm=T), NA)) %>%
  filter(!is.na(species)) # removing Hyla spp. and Hypopachus spp. traits
# warnings are from Tmerge == overwinters
View(tbif)
  
trait_long %>% filter( species == "Lithobates sphenocephalus", grepl('gosner', lifeStage))

full_join(tchar, tbif, by=c('species','lifeStage')) %>%
  mutate(refIDs=paste(refIDs_n, refIDs_a, sep=', ')) %>%
  select(-refIDs_a, -refIDs_n, -lifeStage) %>%
  select(species, everything()) %>%
  write.csv('ATraiU 2.0/ATraiU2_summary_values_2022JAN.csv')

#Table 1----
trait_long %>%
  group_by(species, traitName) %>%
  filter(lifeStage=='adult',
         !is.na(traitValue)) %>%
  slice(1) %>%
  group_by(species) %>%  count() %>%
  right_join(anuran.gbif.taxa %>%
               filter(species %in% gsub(' MNM traits', '', sp_files$name)) %>%
               select(family, genus, species) %>%
               group_by(species) %>% slice(1)) %>%
  select(family, genus, species, n) %>%
  arrange(family) %>%
  write.csv('ATraiU 2.0/manuscript/table_1_220121.csv', row.names = F)


# Outlier Analysis for QA/QC -----
outliers<-bind_rows(
  # all taxa for each trait
trait_long %>%
  filter(traitName != 'Activity') %>%
  group_by(traitName, lifeStage) %>%
  mutate(tV=as.numeric(traitValue),
         z_score=(mean(tV)-tV)/sd(tV)) %>%
  select(species, traitName, traitValue, tV, z_score, traitSource) %>%
  filter(abs(z_score) > 2.49) %>%
  mutate(outlier = 'trait'),
# at the family level
trait_long %>%
  filter(traitName != 'Activity') %>%
  group_by(traitName, family, lifeStage) %>%
  mutate(tV=as.numeric(traitValue),
         z_score=(mean(tV)-tV)/sd(tV)) %>%
  select(species, traitName, traitValue, tV, z_score, traitSource) %>%
  filter(abs(z_score) > 2.49) %>% ungroup() %>%
  mutate(outlier = 'family'),
# at the genus level
trait_long %>%
  filter(traitName != 'Activity') %>%
  group_by(traitName, genus, lifeStage) %>%
  mutate(tV=as.numeric(traitValue),
         z_score=(mean(tV)-tV)/sd(tV)) %>%
  select(species, traitName, traitValue, tV, z_score, traitSource) %>%
  filter(abs(z_score) > 2.49) %>% ungroup() %>%
  mutate(outlier = 'genus'),
# at the species level
trait_long %>%
  filter(traitName != 'Activity') %>%
  group_by(traitName, species) %>%
  mutate(tV=as.numeric(traitValue),
         z_score=(mean(tV)-tV)/sd(tV)) %>%
  select(species, traitName, traitValue, tV, z_score, traitSource) %>%
  filter(abs(z_score) > 2.49) %>%
  mutate(outlier = 'species'))
View(outliers %>%
       group_by(species, traitName, traitValue) %>%
       summarize(z=paste(round(z_score,1), collapse=', '),
                 outs=paste(outlier, collapse=', '),
                 source=paste(unique(traitSource), collapse=', ')))
nrow(outliers %>% distinct(species, traitName, traitValue))
nrow(outliers %>% distinct(species, traitName, traitValue))/ nrow(trait_long %>% filter(traitName !='Activity'))*100
# checked each value was transcribed correctly

trait_long %>%
  # bringing in the outliers flag
  left_join(outliers %>%
              group_by(species, traitName, traitValue) %>%
              summarize(outlier=paste(outlier, collapse=', ')), .groups='drop') %>%
  
  write_csv('ATraiU 2.0/ATraiU2_full_2022JAN_beep.csv')


