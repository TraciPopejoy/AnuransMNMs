# bash script to programatically run Mechanistic Niche models ----
# by Traci DuBose, last edited 11/01/2022

#interact -A usgs_rcs -t 2:00:00 -p normal_q -n 2 # code to start an interactive session

# set up the libraries
.libPaths(.libPaths()[3:1])
#library(devtools)
#devtools::install_github('mrke/NicheMapR')
#devtools::install_github('ilyamaclean/microclima')
library(tidyverse); library(sf); library(parallel); library(NicheMapR); library(hoardr)

# Data Input ------------
# paths
PATH <- "./" #arc
# traits used to parameterize the ectotherm function
traits <- read.csv(paste0(PATH, "inputed_physio_traits.csv")) %>%
  mutate(across(c('s.fossorial', 's.arboreal', 'nocturnal','diurnal','crepuscular'),
                as.numeric)) 
focal_spp<-traits %>% pull(species) %>% unique() # focal species to run models for
# where to save the output
PATH_out<-paste0(PATH, "results")

# Build Points at which to run MNMs ------------
# create raster of study extent
se_r<-st_rasterize(se, dx=1, dy=1) %>% #turn into a raster of 1x1 kilometers
  st_as_sf(as_points = FALSE, merge = FALSE) %>%  #turn into a polygon
  rowid_to_column() # add an id column

se_grid<-NULL
for(s in focal_spp){
  spp_aoo<-read_sf(RCS_resub/L48_1km) %>%
    st_buffer(4) 
  spp_grid<-st_join(se_r, spp_aoo) %>% filter(!is.na(rowid))
  se_grid <- bind_rows(se_grid, spp_grid)
}

# identify all species in a single grid
se_grid_spp <- se_grid %>% as_tibble() %>% # avoiding summarize on sf
  group_by(rowid) %>% 
  summarize(spp_all=paste(unique(pt_rw$species), collapse=', '))

# get the centroid of each grid cell as a point
pts<-se_grid %>% st_centroid() %>%
  left_join(se_grid_spp, by='rowid')
# convert the grid of points to a dataframe for NicheMapR
# locational input into the microclimate function below
se_pts_df <- pts %>% st_coordinates() %>% 
  bind_cols(rowid = pts$rowid, spp_all=pts$spp_all)
write.csv(se_pts_df, paste0('points_ran_', format(Sys.Date()), '.csv'))

# FUNCTION WHICH WE USE TO PARALLALIZE CODE
# it should run the microclimate model and then an ectotherm model for each species found in that grid
anuranMNMs<-function(rw){
  pt<-se_pts_df[se_pts_df$rowid==rw,]
  cat(paste0(rw, 'row number \n'))
  ecto_out <- NULL
  ERR <- 1.5
  micro <- micro_ncep(loc = c(as.numeric(pt[1,2]),as.numeric(pt[1,3])),
                     dstart = "01/01/2020", dfinish = "31/12/2020",
                     DEP = c(0, 2.5,  5,  10,  15,  20,  30,  50, 75,  100), #cm
                     minshade = 0, maxshade = 90, runshade=1,
                    # soilgrids=1,
                     spatial='./ncep_data/ncep_data',
   		     dem.res=1000, # requested resolution DEM from elevatr in meters
                     Usrhyt = 0.01, # local height for organism
		     ERR = ERR)
cat(paste0(min(micro$metout[,1]), ' min micro for ', rw, ' \n'))
  gc()
  while(min(micro$metout[,1])==0 & ERR <= 5){
        cat("model crashed, trying a higher error tolerance \n")
        ERR <- ERR + 0.25
		# rerun the microclimate with slightly higher error tolerance allowed
  	micro <- micro_ncep(loc = c(as.numeric(pt[1,2]),as.numeric(pt[1,3])),
                     dstart = "01/01/2020", dfinish = "31/12/2020",
                     DEP = c(0, 2.5,  5,  10,  15,  20,  30,  50, 75,  100), #cm
                     minshade = 0, maxshade = 90, runshade=1,
                    #soilgrids=1,
                     spatial='./ncep_data/ncep_data',
                     dem.res=1000, # requested resolution DEM from elevatr in meters
                     Usrhyt = 0.01, # local height for organism
                     ERR = ERR)
        gc()
      }
cat('out of while')
  for(spp in focal_spp){
    if(grepl(spp, pt$spp_all)){
      spp_traits <- traits %>% filter(species == spp)
      
      ecto<-ectotherm(Ww_g=spp_traits["Mass"], 
                      shape = 4, # shape based on leopard frog
                      CT_max=spp_traits["CTmax"], 
                      CT_min=spp_traits["CTmin"], 
                      T_pref=spp_traits["Tpref"],
                      # when is activity allowed
                      diurn=spp_traits["diurnal"],
                      nocturn=spp_traits["nocturnal"],
                      crepus=spp_traits["crepuscular"],
                      # can it go into a burrow or climb to cool off
                      burrow=spp_traits["s.fossorial"],
                      climb=spp_traits["s.arboreal"],
                      shdburrow = 1, #the animal's retreat is in the open (0), in the shade when above or below CTmin in sun (1) or in shade always
                      maxdepth = 10, #maximum depth of the burrow
                      T_F_min=max(c(as.numeric(spp_traits["Tforage_min"]), as.numeric(spp_traits["Tmerge"]))),
                      T_F_max=min(c(as.numeric(spp_traits["Tforage_max"]), as.numeric(spp_traits["CTmax"]))),
                      T_RB_min=spp_traits["Tmerge"],
                      T_B_min=spp_traits["Tmerge"],
                      # microclimate port from parent environment
                      nyears = micro$nyears, 
                      minshades = micro$minshade, 
                      maxshades = micro$maxshade, 
                      alpha_sub = (1 - micro$REFL), 
                      DEP = micro$DEP, KS = micro$KS, b = micro$BB, 
                      PE = micro$PE, metout = micro$metout, shadmet = micro$shadmet, 
                      soil = micro$soil, shadsoil = micro$shadsoil, soilmoist = micro$soilmoist, 
                      shadmoist = micro$shadmoist, humid = micro$humid, shadhumid = micro$shadhumid, 
                      soilpot = micro$soilpot, shadpot = micro$shadpot, tcond = micro$tcond, 
                      shadtcond = micro$shadtcond, rainfall = micro$RAINFALL, 
                      preshr = rep(101325 * ((1 - 
                                                (0.0065 * as.numeric(micro$elev)/288))^(1/0.190284)), 
                                   nrow(micro$metout)), elev = as.numeric(micro$elev), 
                      longitude = as.numeric(micro$longlat[1]), 
                      latitude = as.numeric(micro$longlat[2]))
      body_temps<-ecto$environ[,1:5] %>% as_tibble() %>%
        mutate(hot_tsm=as.numeric(spp_traits["CTmax"])-TC) %>% 
        group_by(DOY) %>% 
        mutate(dayrange=max(TC)-min(TC)) %>%
        ungroup() %>%
        summarize(n_total=n(),
                  n_above=sum(hot_tsm < 0),
                  max_TC=round(max(TC),4),
                  tsm=round(min(hot_tsm),4),
                  max_dayrange=round(max(dayrange),4),
                  avg_dayrange=round(mean(dayrange),4),
                  species=spp)
      ecto_out<-bind_rows(ecto_out,
                          body_temps %>% mutate(rowid=rw, err=ERR))
    }
  }
  return(ecto_out)
}

results <- mclapply(se_pts_df[sample(1:nrow(se_pts_df),100),]$rowid, anuranMNMs, mc.cores = 2) # outputs list from each rep of function I think
save_this <- do.call('rbind', results)
# save the outputs of this code
write.csv(save_this, file = paste0(PATH_out, "_", Sys.Date(), ".csv"), row.names=T)
cat('Done')
