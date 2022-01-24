# Quantifying results of Anuran Physiological Trait Database Search
library(tidyverse)

trait_long<-read_csv('ATraiU 2.0/ATraiU2_full_2022JAN.csv') %>%
  filter(!(traitName %in% c("Water Loss Rate",
                        "Minimum Egg Development Temperature",
                        "Maximum Egg Development Temperature",
                        "Metabolic Rate")))
refs <- read_csv('ATraiU 2.0/ATraiU2_reference_list_2022JAN.csv')

# Summary Stats ----
nrow(trait_long)

trait_long %>% pull(refID) %>% unique() %>% length()
trait_long %>% filter(dataCollationType=='concentrated') %>%
  pull(refID) %>% unique() %>% length()

trait_long %>% filter(dataCollationType=='concentrated') %>% nrow()

# most traits
trait_long %>%
  filter(dataCollationType=='concentrated') %>%
  group_by(species, traitName) %>%
  slice(1) %>%
  group_by(traitName) %>%
  tally() %>% arrange(desc(n))

# most categories in concentrated results
rcs<-read_csv('../National-RCS/rcs_results/Anuran RCS values 20211215.csv') %>% 
  filter(grepl('entire', spatial_extent)) %>%
  select(species, orig.nrow, watershed, log10_WS_sqkm)
foc_taxa<-gsub(' MNM traits','', sp_files$name)
#rank geo rarity for table 2
rcs %>% filter(species %in% foc_taxa) %>% 
  mutate(rankgeo=rank(watershed)) %>%
  View()

con_trait_total<-trait_long %>%
  filter(dataCollationType=='concentrated') %>%
  filter(species %in% foc_taxa) %>%
  mutate(lifeStage = case_when(grepl('gosner', lifeStage) ~ 'tadpole',
                                  lifeStage=='unknown' ~ 'adult',
                                  T ~ lifeStage)) %>%
  distinct(species, traitName, lifeStage, .keep_all=T) %>%
  group_by(species) %>%
  tally() %>% ungroup() %>%
  arrange(desc(n))
sp_extremes<-con_trait_total %>%
  filter(n == min(n)|n== max(n))
sp_extremes
trait_long %>% filter(species=='Acris crepitans', dataCollationType=='concentrated') 
con_trait_total %>% 
  summarize(median(n), IQR(n))

# how many did we find haphazardly?
(hap_foc<-trait_long %>%
  filter(dataCollationType=='haphazard',
         species %in% foc_taxa) %>%
  mutate(lifeStage = case_when(grepl('gosner', lifeStage) ~ 'tadpole',
                               lifeStage=='unknown' ~ 'adult',
                               T ~ lifeStage)) %>%
  distinct(species, traitName, lifeStage) %>%
  anti_join(trait_long %>% filter(dataCollationType=='concentrated') %>%
              distinct(species, traitName, lifeStage))%>%
  group_by(species) %>%
  tally() %>% ungroup() %>%
  arrange(desc(n)))
sum(hap_foc$n)

trait_long %>%
  filter(species %in% foc_taxa) %>%
  distinct(species, traitName) %>%
  count(species) %>% filter(n==max(n)|n==min(n))

trait_long %>%
  distinct(species, traitName, lifeStage,  .keep_all=T) %>%
  #filter(species %in% sp_extremes$species) %>%
  group_by(#species,
           lifeStage) %>%
  count() %>% arrange(desc(n))
trait_long %>%
  filter(grepl('capito', species)|grepl('areolatus', species)) %>%
  select(species, traitName, lifeStage, traitValue, refID)

trait_long %>% filter(!grepl('gosner', lifeStage)) %>% 
  pull(lifeStage) %>% unique()
trait_total<-trait_long %>%
  filter(species %in% foc_taxa, 
         lifeStage %in% c('adult','unknown','juvenile','metamorph')) %>%
  #mutate(lifeStage = case_when(grepl('gosner', lifeStage) ~ 'tadpole',
  #                                lifeStage=='unknown' ~ 'adult',
  #                                T ~ lifeStage)) %>%
  distinct(species, traitName, .keep_all=T) %>%
  group_by(species) %>%
  tally() %>% ungroup() %>%
  arrange(desc(n))

foc_taxa[which(!(foc_taxa %in% trait_total$species), useNames = T)]

trait_long %>%
  filter(species %in% foc_taxa) %>%
  mutate(lifeStage = case_when(grepl('gosner', lifeStage) ~ 'tadpole',
                                  lifeStage=='unknown' ~ 'adult',
                                  T ~ lifeStage)) %>%
  distinct(species, traitName)

# look at species of conservation concern
sp_concon<-c('Lithobates areolatus','Lithobates capito')
trait_total %>% filter(species %in% sp_concon)
max(trait_total$n)

?binom.test
binom.test(4,9)
binom.test(2,9)


trait_count_rarity %>%
  ggplot()+
  geom_histogram(aes(x=n), binwidth = 1)+
  geom_vline(data=. %>% filter(species %in% sp_concon), 
             aes(xintercept=n),
             linetype='dashed', size=1.5)+
  geom_text(data=. %>% filter(species %in% sp_concon), 
            aes(x=n+0.1, y=5.3, label=species),
            hjust=0)+
  scale_x_continuous('N Traits',
                     breaks = seq(0,9,1),
                     expand = c(0,0))+
  scale_y_continuous('N species')+
  theme_classic()

# Comparisons among groups ----
#species with no traits
# data frame of n traits for each of our focal taxa
trait_count_rarity<-data.frame(species=foc_taxa) %>%
  filter(!(species %in% trait_total$species)) %>%
  mutate(n=0)%>%
  bind_rows(trait_total) %>%
  left_join(rcs) %>%
  left_join(taxa_key %>% distinct(species, .keep_all=T))
nrow(trait_count_rarity)

# taxonomic comparisons ---
set.seed(12345)

View(trait_count_rarity)
#family comparison
trait_count_rarity %>%
  group_by(family) %>%
  summarize(n_sp=n(),
            med_trait_n=median(n),
            iqr_traits=IQR(n),
            range_traits=paste(min(n), max(n), sep=' to '))
?kruskal.test()
kruskal.test(n~family, data=trait_count_rarity)

#install.packages('MetBrewer')
library(MetBrewer)
gen_pal<-met.brewer('Veronese', n=length(unique(trait_count_rarity$family)),
                    type='discrete')
fam_ord<-trait_count_rarity %>%
  group_by(family) %>%
  summarize(n_sp=n(),
            med_trait_n=median(n)) %>%
  arrange(desc(med_trait_n)) %>%
  pull(family)

fam_plot<-trait_count_rarity %>%
  mutate(famF=factor(family, levels=rev(fam_ord))) %>%
ggplot(aes(x=n, y=famF))+
  geom_boxplot(#data=. %>% 
                 # don't want lines for family with only 1 taxa
                # filter(!(family %in% c('Scaphiopodidae', 'Microhylidae'))),
               aes(color=family), outlier.alpha=0)+
  geom_point(position=position_jitter(height = .3, width=0),
             pch=1, size=2)+
  #geom_text(data=. %>% group_by(famF) %>% count(),
  #          aes(x=9.3,label=n), color='black')+
  labs(y='Family')+
  scale_x_continuous('N Traits', breaks=seq(0,8,2))+
  scale_color_manual('Family', values=gen_pal)+
  theme_classic()+
  theme(legend.position='none')
fam_plot
#genera comparison
trait_count_rarity %>%
  group_by(genus) %>%
  summarize(n_sp=n(),
            med_trait_n=median(n),
            iqr_traits=IQR(n),
            range_traits=paste(min(n), max(n), sep=' to '))
kruskal.test(n~genus, data=trait_count_rarity)
library(dunn.test)
dunn.test(trait_count_rarity$n,
          trait_count_rarity$genus,
          method="bonferroni", alpha=0.05)
gen_ord<-trait_count_rarity %>%
  group_by(genus) %>%
  summarize(n_sp=n(),
            med_trait_n=median(n)) %>%
  arrange(desc(med_trait_n)) %>%
  pull(genus)
#gen_group_mem <- gen_group_mem %>% bind_cols(genus=names(gen_group_mem)) %>%
#  rename('group'=`...1`) %>%
#  mutate(genF=factor(genus, levels=rev(gen_ord)))

gen_plot<-trait_count_rarity %>%
  mutate(genF=factor(genus, levels=rev(gen_ord))) %>%
  ggplot(aes(x=n, y=genF, color=family))+
  geom_point(position=position_jitter(height = .3, width=0),
             size=2, pch=1)+
  geom_boxplot(data=. %>% 
                 # don't want lines for family with only 1 taxa
                 filter(!(genus %in% c('Scaphiopus', 'Gastrophryne'))),
               fill=NA, outlier.alpha=0)+
  #geom_text(data=. %>% group_by(genF) %>% count(),
  #          aes(x=9.3,label=n), color='black')+
  #geom_text(data=gen_group_mem, aes(x=9.5, label=group), color='black',
  #          size=3)+
  geom_text(aes(x=8.2, y='Pseudacris', label='*'), color='black')+
  
  labs(y='Genus')+
  scale_x_continuous('N Traits', breaks=seq(0,8,2))+
  scale_color_manual('Family',values=gen_pal)+
  theme_classic()+
  theme(legend.text = element_text(size=8),
        legend.position='bottom')
gen_plot

library(cowplot)
legend_b <- get_legend(gen_plot)
plot_grid(plot_grid(gen_plot+theme(legend.position='none'), 
                    fam_plot, 
                    rel_widths = c(.55,.45), labels='AUTO'), 
          legend_b, ncol = 1, rel_heights = c(1, .1))
ggsave('ATraiU 2.0/manuscript/taxonomic_comparisons.jpg', 
       width = 6.5, height = 2.5)

# trait and rarity correlation ----
?cor.test
cor.test(trait_count_rarity$log10_WS_sqkm,
         trait_count_rarity$n, method='spearman')
library(scales)
tcor_plot<-trait_count_rarity %>%
  ggplot()+
  geom_point(aes(x=log10_WS_sqkm, y=n))+
  scale_x_continuous(expression('Area of Occupancy, km '^2),
                     breaks=c(3.30103,4,4.69897,5.69897),
                     labels = function(x){comma(10^x)},
                     limits = c(3.30103, 5.7))+
  scale_y_continuous('N Adult Traits')+
  theme_classic()+
  theme(axis.text.x = element_text(angle=20, hjust=.9))
tcor_plot

trait_ls_total<- trait_long %>%
  filter(species %in% foc_taxa) %>%
  mutate(lifeStage = case_when(grepl('gosner', lifeStage) ~ 'tadpole',
                                  lifeStage=='unknown' ~ 'adult',
                                  T ~ lifeStage)) %>%
  #pull(lifeStage) %>% unique()
  distinct(species, traitName, lifeStage, .keep_all=T) %>%
  group_by(species) %>%
  arrange(lifeStage) %>%
  mutate(l_abrev=toupper(substr(lifeStage, 1,1)))%>%
  summarize(n=n(),
            total_ls=paste(unique(l_abrev), collapse=', '),
            n_ls=n_distinct(l_abrev)) %>% 
  ungroup() %>%
  arrange(desc(n)) %>% 
  bind_rows(data.frame(species=foc_taxa) %>%  mutate(n=0)) %>%
  group_by(species) %>% arrange(desc(n)) %>% slice(1) %>%
  left_join(rcs)

nrow(trait_ls_total)
View(trait_ls_total)
unique(trait_ls_total$total_ls)

cor.test(trait_ls_total$n, trait_ls_total$log10_WS_sqkm, method='spearman')

ls_cor_plot<-trait_ls_total %>%
  replace_na(list(n_ls=0))%>%
  ggplot()+
  geom_point(aes(x=log10_WS_sqkm, y=n, fill=as.character(n_ls),
                 color=as.character(n_ls), shape=as.character(n_ls)),
             size=2, alpha=0.75)+
  scale_x_continuous(expression('Area of Occurrence, km '^2),
                     breaks=c(3.30103,4,4.69897,5.69897),
                     labels = function(x){comma(10^x)},
                     limits = c(3.30103, 5.7))+
  scale_y_continuous('N Traits', breaks=c(0,2,4,6,8,10))+
  scale_color_manual('Life\nStages',values=met.brewer('VanGogh1',5),
                     aesthetics = c('color','fill'))+
  scale_shape_manual('Life\nStages',values=c(21,22,24,25,23))+
  theme_classic()+
  theme(#legend.position = c(.15,.7),
        legend.text = element_text(size=8),
        legend.title = element_text(size=8),
        axis.text.x = element_text(angle=20, hjust=.9))
ls_cor_plot

plot_grid(tcor_plot, ls_cor_plot, labels='AUTO',rel_widths = c(.45,.55))
ggsave('ATraiU 2.0/manuscript/trait_aoo_correlation.jpg', width=4.75, height=2.5)

trait_ls_total %>% filter(n_ls > 2) %>% select(-orig.nrow)
trait_ls_total %>% filter(n_ls <= 2) %>% select(-orig.nrow, log10_WS_sqkm)
trait_ls_total %>% filter(grepl('T', total_ls)) %>% nrow()
# number of 
nrow(trait_long)
trait_long %>%
  mutate(new_ls=ifelse(grepl('gosner',lifeStage), 'tadpole',lifeStage)) %>%
  #filter(is.na(new_ls)) %>% 
  count(new_ls) %>% arrange(desc(n))
length(unique(trait_long$species)) #through haphazard search
foc_taxa<-trait_long %>%
  filter(dataCollationType=='concentrated') %>%
  pull(species) %>% unique()
length(foc_taxa)
tr_sp_con<-trait_long %>%
  filter(species %in% foc_taxa) %>%
  group_by(species, traitName) %>% slice(1) %>% #View()
  group_by(species) %>% count() %>% arrange(desc(n))
tr_sp_con %>% ungroup() %>% filter(n==max(n)|n==min(n))
tr_sp_con %>%
  ggplot()+geom_histogram(aes(x=n), binwidth=1)+
  scale_x_continuous('n Traits per species', 
                      breaks=1:11)+
  scale_y_continuous('N species', expand=c(0,0))+
  theme_minimal()
ggsave('AT2_fig/trait_per_sp_conc.jpeg', width=2.5, height=2.5)

trait_order<-trait_long %>%
  group_by(species, traitName) %>% slice(1) %>%
  group_by(traitName) %>% count() %>%
  arrange(desc(n)) %>%
  pull(traitName)

trait_long %>%
  group_by(species, traitName) %>% slice(1) %>%
  group_by(traitName) %>% count() %>%
  filter(!(traitName %in% c("Maximum Egg Development Temperature",
                        "Minimum Egg Development Temperature",
                        "Standard Metabolic Rate",
                        "VTmax","VTmin",
                        "Cutaneous Water Loss Rate",
                        "Thermal Perfomance Curve",
                        "Metabolic Rate"))) %>%
  mutate(TraitF=factor(traitName, levels = rev(trait_order))) %>%
  ggplot()+
  geom_col(aes(x=TraitF, y=n))+
  theme_minimal()+
  labs(x='Traits',y='Species')+
  theme(axis.text = element_text(size=9))+
  coord_flip()
ggsave('AT2_fig/trait_distribution.jpg', width=2.5, height=2.5)

trait_long %>% group_by(species, traitName) %>% slice(1) %>% ungroup() %>% count(dataCollationType)

#pivot_wider traits

#sp x trait count matrix
trait_long %>%
  filter(lifeStage %in% c('unknown', 'adult')) %>% 
  select(species, traitName, group_id) %>%
  pivot_wider(names_from = traitName, values_from=group_id, values_fn=length)

#summarized matrix
trait_long %>%
  filter(lifeStage %in% c('unknown', 'adult')) %>%
  
  
  # Tile Plot ----
#RCS for ordering
RCS<-read.csv('../National-RCS/rcs_results/Anuran RCS values 20210405.csv') %>%
  group_by(species) %>% slice(1)

trait_long %>%
  group_by(species, traitName) %>%
  filter(!is.na(Value),
         species %in% foc_taxa,
         !(traitName %in% c("Maximum Egg Development Temperature",
                        "Minimum Egg Development Temperature",
                        "Standard Metabolic Rate",
                        "VTmax","VTmin","Water Loss Rate",
                        "Cutaneous Water Loss Rate",
                        "Thermal Perfomance Curve",
                        "Metabolic Rate"))) %>%
  slice(1)%>%
  select(species, traitName) %>%
  mutate(presence=1,
         TraitF=factor(traitName, levels = trait_order)) %>% 
  pivot_wider(names_from=species, values_from=presence, values_fill=0) %>%
  ungroup() %>% select(-traitName) %>%
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

auth_ord<-trait_long %>% count(first_author) %>% arrange(desc(n)) %>% pull(first_author)
trait_long %>%
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
trait_long %>%
  group_by(group_id, year, traitName) %>% count() %>%
  mutate(simple_TC=ifelse(traitName %in% c("Activity","CTmax","CTmin","Mass","Tmerge","Tpref"),
                          traitName, 'Other')) %>%
  ggplot()+geom_histogram(aes(x=year, fill=simple_TC), binwidth = 1)+
  scale_x_continuous(breaks = seq(1935,2020,5))+
  scale_fill_viridis_d()+
  theme_classic()+
  theme(axis.text.x = element_text(angle=90),
        legend.position = 'bottom')
ggsave('AT2_fig/ref_year_color.jpg', width=5, height=2)

refs_df %>% arrange(year)

trait_long %>%
  mutate(lfstage=ifelse(lifeStage=='tapdole' | grepl('gosner', lifeStage), 'tadpole', lifeStage),
         lfstage2=recode(lfstage, 'juveniles'='juvenile',
                         'unkown'='unknown')) %>%
  group_by(species, lfstage2, traitName) %>%
  slice(1) %>%
  group_by(species, lfstage2) %>%
  count() %>% arrange(desc(n)) %>%
  filter(n > 2) %>%
  ggplot()+
  geom_bar(aes(x=species, y=n, fill=lfstage2), 
           position='stack', stat='identity')+ coord_flip()+
  scale_fill_viridis_d(na.value='lightgrey')+theme_bw()

trait_long %>%
  mutate(lfstage=ifelse(lifeStage=='tapdole' | grepl('gosner', lifeStage), 'tadpole', lifeStage),
         lfstage2=recode(lfstage, 'juveniles'='juvenile',
                         'unkown'='unknown')) %>%
  group_by(genus, lfstage2) %>%
  count() %>% arrange(desc(n)) %>%
  filter(n > 5) %>%
         ggplot()+
           geom_bar(aes(x=genus, y=n, fill=lfstage2), 
                    position='stack', stat='identity')+ coord_flip()+
           scale_fill_viridis_d(na.value='lightgrey')+theme_bw()
         
  