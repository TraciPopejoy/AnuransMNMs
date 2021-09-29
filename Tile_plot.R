
# Tile Plot ----
#RCS for ordering
RCS<-read.csv('../National-RCS/rcs_results/Anuran RCS values 20210405.csv') %>%
  group_by(species) %>% slice(1)

all_trait_shts %>%
  group_by(taxa, Trait) %>%
  filter(!is.na(val)) %>%
  filter(!(Trait %in% c("Maximum Egg Development Temperature",
                        "Minimum Egg Development Temperature",
                        "Standard Metabolic Rate",
                        "VTmax","VTmin","Water Loss Rate"))) %>%
  slice(1)%>%
  select(taxa, Trait) %>%
  mutate(presence=1) %>% 
  pivot_wider(names_from=taxa, values_from=presence, values_fill=0) %>%
  pivot_longer(-Trait) %>%
  mutate(name=recode(name, `Hyla chrysoscelis`="Dryophytes chrysoscelis",
                     `Hyla cinerea`="Dryophytes cinereus",
                     `Hyla squirella`="Dryophytes squirellus"),
         taxaF=factor(name, levels=RCS$species[order(RCS$RCS_WS)])) %>%
  left_join(RCS, by=c('name'='species')) %>%
  #View()
  ggplot()+
  geom_tile(aes(x=Trait, y=taxaF, fill=as.factor(value))) +
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
length(unique(all_trait_shts$taxa))
all_trait_shts %>% filter(!is.na(`Source file name`)) %>%
  count(`Source file name`) %>%
  arrange(desc(n)) %>%
  View()
write.csv(all_trait_shts, paste0('all_traits_', format(Sys.Date(), "%Y%m%d"), '.csv'),
          row.names=F)
