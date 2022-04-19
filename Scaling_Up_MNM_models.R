library(tidyverse);library(sf);library(stars)
#STEPS
# identify study extent
library(maps)
va<-st_as_sf(map('state','virginia', fill=T, plot=F)) %>%
  st_transform(st_crs("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=km +no_defs"))
# create raster of study extent
st_crs(va)
va_r<-st_rasterize(va, dx=50, dy=50) %>% #turn into a raster
  st_as_sf(as_points = FALSE, merge = FALSE) #turn into a polygon

traits<-read.csv('ATraiU 2.0/ATraiU2_summary_values_2022JAN.csv')
names(traits)
traits_used <- traits %>% filter(species %in% c('Anaxyrus americanus','Lithobates clamitans'))

imp_traits <- read.csv('imputed_traits_041922.csv') %>%
  select(species, variable, Yes) %>%
  pivot_wider(names_from=variable, values_from=Yes)

# run microclimate on points
library(NicheMapR)
ecto_out<-NULL
start.time<-Sys.time()
for(spp in c('Anaxyrus americanus','Lithobates clamitans')){
  # idea: use one point per raster; point will be real occurrence
  anax<-read.csv(paste0('../National-RCS/data/occ_data/',spp,'_20210330.csv')) %>%
    st_as_sf(coords=c('Longitude','Latitude'), 
             crs=st_crs("+proj=longlat +ellps=WGS84 +datum=WGS84"))
  anax_1per<-st_join(anax, va_r %>% 
                       st_transform("+proj=longlat +ellps=WGS84 +datum=WGS84") %>%
                       rowid_to_column()) %>%
    filter(!is.na(rowid)) %>%
    group_by(rowid) %>% sample_n(1)
  #print(ggplot()+
  #        geom_sf(data=va_r)+
  #        geom_sf(data=anax_1per))
  spp_code<-paste0(substr(gsub('\\ .*','',spp),1,2),'_', 
                   substr(gsub('.*\\ ','',spp),1,4))
  va_pts<-anax_1per %>% st_transform("+proj=longlat +ellps=WGS84 +datum=WGS84") %>%
    st_coordinates()
  write.csv(va_pts, paste0('micro_climate_outputs/',spp,'points_run.csv'))
  
  # pull species traits
  spp_traits<-imp_traits %>% filter(species==spp) %>%
    left_join(traits_used %>% dplyr::select(species:crepuscular))
  
  #micro_clim_summary<-NULL
  for(i in 1:10){
    #run the micro climate data at each row
    micro<-micro_usa(loc = c(va_pts[i,1],va_pts[i,2]),
                     dstart = "01/01/2020", dfinish = "31/12/2020", 
                     DEP = c(0, 2.5,  5,  10,  15,  20,  30,  50,  100,  200), #cm
                     minshade = 0, maxshade = 90,
                     Usrhyt = 0.01)
    #micro_clim_summary<-bind_rows(micro_clim_summary,
    #                              data.frame(point_lat=micro$longlat[2],
    #                                         point_long=micro$longlat[1],
    #                                           for_i=i,
    #                                          elevation=micro$elev,
    #                                         max_rain=max(micro$RAINFALL),
    #                                        max_1cm_temp=max(micro$metout[,3]),
    #                                       max_2m_temp=max(micro$metout[,4])))
    ecto<-ectotherm(Ww_g=spp_traits["Mass"], 
                    CT_max=spp_traits["CTmax"], 
                    CT_min=spp_traits["CTmin"], 
                    T_pref=spp_traits["Tpref"],
                    diurn=spp_traits["diurnal"],
                    nocturn=spp_traits["nocturnal"],
                    crepus=spp_traits["crepuscular"],
                    burrow=1,
                    climb=0,
                    shdburrow = 1, #the animal's retreat is in the open (0), in the shade when above or below CTmin in sun (1) or in shade always
                    maxdepth = 2, #maximum depth of the burrow
                    T_F_min=19,
                    T_F_max=35,
                    T_RB_min=spp_traits["Tmerge"],
                    T_B_min=spp_traits["Tmerge"])
    #saveRDS(ecto,
    #        paste0('micro_climate_outputs/PT_row',
    #               u,'ecto.rds'))
    # quantify   
    body_temps<-ecto$environ[,1:5]
    ecto_out<-bind_rows(ecto_out,
                        body_temps %>% as_tibble() %>%
                          mutate(hot_TC=spp_traits["CTmax"]-TC) %>%
                          filter(TC > spp_traits["CTmax"]) %>%
                          summarize(n_above=n(),
                                    max_TC=max(TC),
                                    tsm=max_TC-spp_traits["CTmax"],
                                    point=i,
                                    X=va_pts[i,1],
                                    Y=va_pts[i,2]))
    
    print(paste(i, 'of', nrow(va_pts)))
  }
  #assign(paste0(spp_code,'_ecto_out'), ecto_out)
  end.time<-Sys.time()
}
#1152
#START OF FOR LOOP
# run ectotherm model & quantify output
ecto_out<-NULL
for(u in 1:3){#nrow(va_pts))[!((1:nrow(va_pts)) %in% not_run_rows)]{
  micro<-read_rds(paste0('micro_climate_outputs/PT_row',
                         u,'.rds'))

}
plot_df<-ecto_out %>% 
  bind_cols(data.frame(spp=rep(c('Anaxyrus americanus','Lithobates clamitans'), each=10))) %>%
  st_as_sf(coords=c('X','Y'),crs="+proj=longlat +ellps=WGS84 +datum=WGS84")

plot_df %>%
  ggplot()+
  geom_sf(data=va_r)+
  geom_sf(aes(color=n_above))+
  facet_wrap(~spp)
library(cowplot)
plot_grid(plot_df %>% ggplot()+
            geom_sf(data=va_r)+
            geom_sf(aes(color=tsm)),
          plot_df %>% ggplot()+
            geom_sf(data=va_r)+
            geom_sf(aes(color=n_above)))
  
se<-st_as_sf(map('state',c('mississippi', 'georgia',
                           'florida','south carolina', 'north carolina', 'virginia',
                           'alabama', 'tennessee'), fill=T, plot=F)) %>%
  st_transform(st_crs("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=km +no_defs"))
ggplot()+geom_sf(data=se)
# create raster of study extent
se_r<-st_rasterize(se, dx=50, dy=50) %>% #turn into a raster
  st_as_sf(as_points = FALSE, merge = FALSE)
ggplot()+geom_sf(data=se_r)
nrow(se_r)


### nice image for SAS presentation----


traits<-read.csv('ATraiU 2.0/ATraiU2_summary_values_2022JAN.csv')
names(traits)
View(traits)
traits_used <- traits %>% filter(species %in% c('Anaxyrus americanus','Lithobates clamitans', 'Scaphiopus holbrookii'))
all_results<-NULL

longlat <- c(-80.41, 37.23)
micro <- micro_global(loc = longlat,
                      timeinterval = 12)
for(i in 1:3){
  ecto<-ectotherm(Ww_g=traits_used[i,"Mass"], 
            CT_max=traits_used[i,"CTmax"], 
            CT_min=-1, 
            T_pref=traits_used[i,"Tpref"],
            diurn=traits_used[i,"diurnal"],
            nocturn=traits_used[i,"nocturnal"],
            crepus=traits_used[i,"crepuscular"],
            burrow=1,
            climb=0,
            shdburrow = 1, #the animal's retreat is in the open (0), in the shade when above or below CTmin in sun (1) or in shade always
            maxdepth = 2, #maximum depth of the burrow
            T_F_min=max(traits_used[i,"Tmerge"],
                        ifelse(is.na(traits_used[i,"Tforage_min"]), 15,
                            traits_used[i,"Tforage_min"])),
            T_F_max=ifelse(is.na(traits_used[i,"Tforage_max"]), 20,
                           traits_used[i,"Tforage_max"]),
            T_RB_min=traits_used[i,"Tmerge"],
            T_B_min=traits_used[i,"Tmerge"])
  new_results<-ecto$environ %>% as_tibble() %>%
    mutate(species=traits_used[i,"species"])
  # save the changes in the outcome
  all_results<-bind_rows(all_results, new_results)
}

head(all_results)
library(lubridate)
body_temp<-as_tibble(all_results) %>%
  mutate(date_x=as_date(DOY-1, origin='2019-01-01'),
         day_time=ymd_h(paste0(date_x, " ", TIME))) 
#body_temp %>% select(YEAR, DOY, TIME, date_x, day_time)
#body_temp %>% pull(date_x) %>% unique()
body_temp %>%
  filter(day_time<'2019-09-01 0:00' & day_time > '2019-08-01 0:00') %>%
  ggplot()+
 # geom_point(aes(x=TIME, y=TC, shape=as.character(ACT)), alpha=0.4)+
  geom_line(aes(x=TIME, y=TC, 
                color=species, group=species),
            size=1.5, alpha=0.4)+
  geom_hline(data=traits_used,
             aes(yintercept=CTmax, color=species),
             size=1.3, linetype='dashed')+
  ggtitle('a day in August in Blacksburg')+
  scale_color_viridis_d(name='Species')+
  scale_y_continuous('Body Temperature °C')+
  scale_x_continuous('Hour of the Day',limits=c(0,23))+
  #scale_shape('Activity', labels=c('inactive','basking', 'foraging'))+
  theme_classic()+
  theme(plot.title = element_text(hjust=0.5),
        strip.text = element_blank(),
        strip.background = element_blank(),
        legend.position='bottom',
        legend.text = element_text(face='italic'))
