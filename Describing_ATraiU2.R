# Quantifying results of Anuran Physiological Trait Database Search
library(tidyverse)

trait_db_raw<-read_csv('denormalized_trait_db.csv')
refs <- read_csv('ATraiU 2.0/Reference_List.csv')

# Summary Stats ----
nrow(trait_db_raw)

trait_db_raw %>% pull(group_id) %>% unique() %>% length()

# most traits
trait_db_raw %>%
  filter(type=='concentrated') %>%
  group_by(species, Trait) %>%
  slice(1) %>%
  group_by(Trait) %>%
  tally() %>% arrange(desc(n))

# most categories in concentrated results
trait_db_raw %>%
  filter(type=='concentrated') %>%
  distinct(species, Trait,  .keep_all=T) %>%
  group_by(species) %>%
  tally() %>% ungroup() %>%
  arrange(desc(n))%>%
  slice(1:3, (n()-6):n())

trait_db_raw %>%
  filter(species %in% c('Pseudacris fouquettei','Pseudacris brimleyi',
                        'Pseudacris ocularis', 'Pseudacris brachyphona',
                        'Lithobates capito')) %>% 
  View()

trait_db_raw %>%
  filter(type=='concentrated') %>%
  group_by(species, Trait, `Life Stage`) %>%
  slice(1) %>%
  group_by(Trait, `Life Stage`) %>%
  tally() %>%
  pivot_wider(names_from=`Life Stage`, values_from=n)

# number of 
nrow(trait_db_raw)
trait_db_raw %>%
  mutate(new_ls=ifelse(grepl('gosner',`Life Stage`), 'tadpole',`Life Stage`)) %>%
  filter(is.na(new_ls)) %>% View()
  count(new_ls) %>% arrange(desc(n))
length(unique(trait_db_raw$species)) #through haphazard search
foc_taxa<-trait_db_raw %>%
  filter(type=='concentrated') %>%
  pull(species) %>% unique()
length(foc_taxa)
trait_db_raw %>%
  filter(species %in% foc_taxa) %>%
  group_by(species, Trait) %>% slice(1) %>% View()
  group_by(species) %>% count() %>%
  ggplot()+geom_histogram(aes(x=n), binwidth=1)+
  scale_x_continuous('n Traits per species', 
                      breaks=1:11)+
  scale_y_continuous('N species', expand=c(0,0))+
  theme_minimal()
ggsave('AT2_fig/trait_per_sp_conc.jpeg', width=2.5, height=2.5)

trait_order<-trait_db_raw %>%
  group_by(species, Trait) %>% slice(1) %>%
  group_by(Trait) %>% count() %>%
  arrange(desc(n)) %>%
  pull(Trait)

trait_db_raw %>%
  group_by(species, Trait) %>% slice(1) %>%
  group_by(Trait) %>% count() %>%
  filter(!(Trait %in% c("Maximum Egg Development Temperature",
                        "Minimum Egg Development Temperature",
                        "Standard Metabolic Rate",
                        "VTmax","VTmin",
                        "Cutaneous Water Loss Rate",
                        "Thermal Perfomance Curve",
                        "Metabolic Rate"))) %>%
  mutate(TraitF=factor(Trait, levels = rev(trait_order))) %>%
  ggplot()+
  geom_col(aes(x=TraitF, y=n))+
  theme_minimal()+
  labs(x='Traits',y='Species')+
  theme(axis.text = element_text(size=9))+
  coord_flip()
ggsave('AT2_fig/trait_distribution.jpg', width=2.5, height=2.5)

trait_db_raw %>% group_by(species, Trait) %>% slice(1) %>% ungroup() %>% count(type)

#pivot_wider traits

#sp x trait count matrix
trait_db_raw %>%
  filter(`Life Stage` %in% c('unknown', 'adult')) %>% 
  select(species, Trait, group_id) %>%
  pivot_wider(names_from = Trait, values_from=group_id, values_fn=length)

#summarized matrix
trait_db_raw %>%
  filter(`Life Stage` %in% c('unknown', 'adult')) %>%
  
  
  # Tile Plot ----
#RCS for ordering
RCS<-read.csv('../National-RCS/rcs_results/Anuran RCS values 20210405.csv') %>%
  group_by(species) %>% slice(1)

trait_db_raw %>%
  group_by(species, Trait) %>%
  filter(!is.na(Value),
         species %in% foc_taxa,
         !(Trait %in% c("Maximum Egg Development Temperature",
                        "Minimum Egg Development Temperature",
                        "Standard Metabolic Rate",
                        "VTmax","VTmin","Water Loss Rate",
                        "Cutaneous Water Loss Rate",
                        "Thermal Perfomance Curve",
                        "Metabolic Rate"))) %>%
  slice(1)%>%
  select(species, Trait) %>%
  mutate(presence=1,
         TraitF=factor(Trait, levels = trait_order)) %>% 
  pivot_wider(names_from=species, values_from=presence, values_fill=0) %>%
  ungroup() %>% select(-Trait) %>%
  pivot_longer(-TraitF) %>%
  mutate(taxaF=factor(name, levels=RCS$species[order(RCS$RCS_WS)])) %>%
  left_join(RCS, by=c('name'='species')) %>%
  filter(!is.na(taxaF)) %>% #View()
  ggplot()+
  geom_tile(aes(x=TraitF, y=taxaF, fill=as.factor(value))) +
  scale_fill_manual(values=c(NA,'black'), name='Trait Available',
                    labels=c('no', 'yes'))+
  scale_y_discrete(name="Taxa")+
  theme_bw()+
  theme(axis.text.x= element_text(angle=40, hjust=.9),
        legend.position = 'bottom',
        axis.title = element_blank(), 
        axis.text.y=element_text(face='italic'),
        legend.margin=margin(-2,-2,-2,-2),
        legend.box.margin=margin(-5,-5,-5,-5))
ggsave('AT2_fig/trait_presence_graph.jpg', width=5, height = 4)
write.csv(all_trait_shts, paste0('all_traits_', format(Sys.Date(), "%Y%m%d"), '.csv'),
          row.names=F)

# Assess References ----
refs_df<-refs %>% group_by(group_id) %>% slice(1) %>% ungroup()
nrow(refs_df)  
refs_df %>% 
  select(ends_with('author')) %>%
  pivot_longer(everything()) %>%
  filter(!is.na(value), value !="") %>%
  count(value) %>%
  arrange(desc(n)) %>% View()

auth_ord<-trait_db_raw %>% count(first_author) %>% arrange(desc(n)) %>% pull(first_author)
trait_db_raw %>%
  count(first_author) %>%
  filter(n>4) %>%
  mutate(auth=factor(first_author, 
                     levels=rev(auth_ord))) %>%
  ggplot()+geom_col(aes(y=auth, x=n))+
  scale_x_continuous(trans='log10', name = 'Traits Extracted',
                     breaks=c(1,5,10,25,50,75))+
  scale_y_discrete('')+
  theme_classic()
ggsave('AT2_fig/trait_author_dist.jpg', width=4, height=6)  
refs_df %>%
  count(journal) %>%
  arrange(desc(n))
refs_df %>% filter(is.na(journal))
trait_db_raw %>%
  group_by(group_id, year, Trait) %>% count() %>%
  mutate(simple_TC=ifelse(Trait %in% c("Activity","CTmax","CTmin","Mass","Tmerge","Tpref"),
                          Trait, 'Other')) %>%
  ggplot()+geom_histogram(aes(x=year, fill=simple_TC), binwidth = 1)+
  scale_x_continuous(breaks = seq(1935,2020,5))+
  scale_fill_viridis_d()+
  theme_classic()+
  theme(axis.text.x = element_text(angle=90),
        legend.position = 'bottom')
ggsave('AT2_fig/ref_year_color.jpg', width=5, height=2)

refs_df %>% arrange(year)

trait_db_raw %>%
  mutate(lfstage=ifelse(`Life Stage`=='tapdole' | grepl('gosner', `Life Stage`), 'tadpole', `Life Stage`),
         lfstage2=recode(lfstage, 'juveniles'='juvenile',
                         'unkown'='unknown')) %>%
  group_by(species, lfstage2, Trait) %>%
  slice(1) %>%
  group_by(species, lfstage2) %>%
  count() %>% arrange(desc(n)) %>%
  filter(n > 2) %>%
  ggplot()+
  geom_bar(aes(x=species, y=n, fill=lfstage2), 
           position='stack', stat='identity')+ coord_flip()+
  scale_fill_viridis_d(na.value='lightgrey')+theme_bw()

trait_db_raw %>%
  mutate(lfstage=ifelse(`Life Stage`=='tapdole' | grepl('gosner', `Life Stage`), 'tadpole', `Life Stage`),
         lfstage2=recode(lfstage, 'juveniles'='juvenile',
                         'unkown'='unknown')) %>%
  group_by(genus, lfstage2) %>%
  count() %>% arrange(desc(n)) %>%
  filter(n > 5) %>%
         ggplot()+
           geom_bar(aes(x=genus, y=n, fill=lfstage2), 
                    position='stack', stat='identity')+ coord_flip()+
           scale_fill_viridis_d(na.value='lightgrey')+theme_bw()
         
  