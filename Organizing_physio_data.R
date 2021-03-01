library(tidyverse);library(googlesheets4); library(googledrive)

sp_files<-drive_find(team_drive = "Anuran Physiological Trait Search", 
           pattern="MNM traits")

all_trait_shts<-NULL

for(i in 1:nrow(sp_files)){
  file.id<-sp_files[[2]][i]
  file.taxa<-sub(" MNM traits", "", sp_files[[1]][i])
  sht<-read_sheet(file.id) %>% 
    mutate(taxa=file.taxa,
           val=as.character(Value),
           doi=as.character(DOI)) %>%
  select(-Value, -DOI)
  all_trait_shts<-bind_rows(all_trait_shts,sht)
}
all_trait_shts %>%
  filter(!is.na(`Source file name`),
         Units !="n/a")

# Tile Plot ----
all_trait_shts %>%
  group_by(taxa, Trait) %>%
  filter(!is.na(`Source file name`),
         Units !="n/a") %>%
  select(taxa, Trait) %>%
  slice(1) %>%
  mutate(presence=1) %>%
  pivot_wider(names_from=taxa, values_from=presence, values_fill=0) %>%
  pivot_longer(-Trait) %>%
  ggplot()+
  geom_tile(aes(x=Trait, y=name, fill=as.factor(value))) +
  scale_fill_manual(values=c('white','black'), name='Trait Available',
                    labels=c('no', 'yes'))+
  scale_y_discrete(name="Taxa")+
  theme(axis.text.x= element_text(angle=40, hjust=.9),
        legend.position = 'bottom',
        axis.title = element_blank())

# Build Reference List ----
# identify ones with doi and cross reference
library(rcrossref)
doi.refs<-all_trait_shts %>% 
  filter(!is.na(doi),
         Units !='n/a',
         !(doi %in% c("NULL", "-", 28))) %>%
  group_by(doi) %>% slice(1) %>%
  select(`Source file name`,doi) %>%
  rowwise() %>%
  mutate(citation = list(cr_cn(dois = sub("DOI ", "", doi), format = "text")))
#will get lots of blue text, just need to hand fix Rausch

# identify the ones I need to look up
look_these_up<-all_trait_shts %>% 
  filter(!is.na(`Source file name`),
         `Source file name` !='n/a') %>%
  group_by(`Source file name`) %>% slice(1) %>%
  select(taxa, `Source file name`, doi) %>%
  filter(is.na(doi) | doi == '-')%>%
  mutate(yr=gsub("[^0-9\\.]", "",  `Source file name`),
         first_auth=sub("[0-9].*", "", `Source file name`))
#for loop to check for references for dois
all_quick<-NULL
for(j in 1:nrow(look_these_up)){
  row_bib<-look_these_up[j,]
  quick_peak<-cr_works(query = "frog",
           flq = c(query.author = row_bib$first_auth, 
                   query.bibliographic = row_bib$yr),
           select = c('DOI', 'title', 'author'))$data %>%
    mutate(first_auth=row_bib$first_auth,
           yr=row_bib$yr)
  all_quick<-bind_rows(all_quick,quick_peak)
}
all_quick %>% left_join(look_these_up, by=c('first_auth','yr'))

# bring it all back together into one reference list
all_trait_shts %>%
  select(taxa, `Source file name`, doi) %>%
  filter(!is.na(`Source file name`),
         `Source file name` !='n/a') %>%
  
