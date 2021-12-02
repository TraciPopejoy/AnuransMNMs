library(sf)
a2_taxa<-c('Anaxyrus americanus', 
           'Anaxyrus fowleri', 'Anaxyrus punctatus', 'Anaxyrus quercicus', 'Anaxyrus terrestris',
           'Acris crepitans', 'Acris gryllus', 'Dryophytes chrysoscelis', 'Dryophytes cinereus',
           'Dryophytes gratiosus', 'Dryophytes squirellus', 'Dryophytes versicolor', 'Pseudacris nigrita',
           'Pseudacris triseriata', 'Pseudacris brachyphona', 'Pseudacris brimleyi', 'Pseudacris feriarum',
           'Pseudacris fouquettei', 'Pseudacris ocularis', 'Pseudacris ornata', 'Gastrophryne carolinensis',
           'Lithobates catesbeianus', 'Lithobates clamitans', 'Lithobates grylio', 'Lithobates palustris',
           'Lithobates sylvaticus','Lithobates areolatus', 'Lithobates capito', 'Scaphiopus holbrookii')
iucn_anuran<-read_sf('ANURA', 'ANURA') %>%
  filter(binomial)

library(fasterize)
se_frogs<-iucn_anuran %>% 
  filter(binomial %in% a2_taxa,
         !(binomial == a2_taxa[22] & origin == 3)) %>%
  #filter(binomial %in% a2_taxa[22]) %>%
  group_by(binomial) %>%
  summarize()
r <- raster(se_frogs, res = .25)
r2<-fasterize(st_cast(se_frogs, 'POLYGON'), r, fun='sum') 
plot(r2)
library(raster)
raster.points <- rasterToPoints(r2)
raster.points <- data.frame(raster.points)
colnames(raster.points) <-c('x','y','layer')

library(maps)
SE_states<-c('mississippi', 'georgia',
             'florida','south carolina', 'north carolina', 'virginia',
             'alabama', 'tennessee')
usa <- st_as_sf(map("usa", plot = FALSE, fill = TRUE))
se_states<-st_as_sf(map("state", plot = FALSE, fill = TRUE))  %>%
  filter(ID %in% SE_states)
an_range<-ggplot()+
  geom_tile(data=raster.points,
          aes(x=x, y=y, fill=layer))+
  geom_sf(data=se_states, 
          color='black', fill=NA)+
  coord_sf(xlim=st_bbox(se_states)[c(1,3)],
           ylim=st_bbox(se_states)[c(2,4)])+
  scale_fill_gradient('species\nrichness',
                      low='grey90', high='grey30',
                      breaks=c(0,4,8,12,16), expand=c(0,0))+
  labs(x="Longitude", y="Latitude")+
  theme_classic()+
  theme(legend.title=element_text(size=8),
        legend.text = element_text(size=8),
        axis.title=element_text(size=9),
        axis.text=element_text(size=8))
ins<-ggplot()+
  geom_sf(data=se_states, color='grey', fill='grey')+
  geom_sf(data=usa, fill=NA, color='black')+
  labs(x="Longitude", y="Latitude")+
  theme_classic()+
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.line = element_blank(),
        axis.ticks= element_blank(),
        panel.border = element_rect(color='black', fill=NA),
        plot.margin = unit(c(0,0,0,0),'inches'))
library(cowplot)
ggdraw() +
  draw_plot(an_range) +
  draw_plot(ins, x = 0.19, y = 0.19, width = 0.23, height = 0.23)
ggsave('AT2_fig/study_species_extent.jpg')
gg_inset_map1