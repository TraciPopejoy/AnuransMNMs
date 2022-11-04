
#interact -A yourallocation -t 2:00:00 -p t4_dev_q -n 1 --gres=gpu:1

# set up the libraries
.libPaths(.libPaths()[3:1])
library(tidyverse);library(sf)

# paths
PATH <- "/AnuranMNMs" #arc

# data input
pts_df_shp <- read_sf(PATH, "points_ran") # points at which to run the MNM model
pts_df1 <- pts_df_shp %>% distinct(geometry, .keep_all=T) 
pts_df <- se_pts_df1 %>% st_coordinates() %>%
  bind_cols(rowid = se_pts_df1$rowid, spp_all=se_pts_df1$spp_all)

spp_traits <- paste0(PATH, "")# species traits to parameterize the MNM model

# where to save the output
PATH_out<-paste0(PATH, "results")

# function to run the ectotherm model
Te_est<-function(spp, save=F){
  spp_traits <- traits %>% filter(species == spp)
    ecto<-ectotherm(Ww_g=spp_traits["Mass"], 
                    CT_max=spp_traits["CTmax"], 
                    CT_min=spp_traits["CTmin"], 
                    T_pref=spp_traits["Tpref"],
                    diurn=spp_traits["diurnal"],
                    nocturn=spp_traits["nocturnal"],
                    crepus=spp_traits["crepuscular"],
                    burrow=spp_traits["s.fossorial"],
                    climb=spp_traits["s.arboreal"],
                    shdburrow = 1, #the animal's retreat is in the open (0), in the shade when above or below CTmin in sun (1) or in shade always
                    maxdepth = 10, #maximum depth of the burrow
                    T_F_min=max(c(19, as.numeric(spp_traits["Tmerge"]))),
                    T_F_max=min(c(35, as.numeric(spp_traits["CTmax"]))),
                    T_RB_min=spp_traits["Tmerge"],
                    T_B_min=spp_traits["Tmerge"])
    if(save==T){saveRDS(ecto,
                paste0('microclimate_data/pt',rw,'/!',spp,'.rds'))}
    body_temps<-ecto$environ[,1:5] %>% as_tibble() %>%
      mutate(hot_tsm=as.numeric(spp_traits["CTmax"])-TC) %>% 
      group_by(DOY) %>% 
      mutate(dayrange=max(TC)-min(TC)) %>%
      ungroup() %>%
      summarize(n_total=n(),
                n_above=sum(hot_tsm < 0),
                max_TC=max(TC),
                tsm=min(hot_tsm),
                max_dayrange=max(dayrange),
                avg_dayrange=mean(dayrange),
                X=micro$longlat[1],
                Y=micro$longlat[2],
                species=spp)
return(output)
}

# FUNCTION WHICH WE USE TO PARALLALIZE CODE
# it should run the microclimate model and then an ectotherm model for each species found in that grid
anuranMNMs<-function(rw=i, savedf=F){
 micro<-micro_ncep(loc = c(as.numeric(pts_df[rw,1]),as.numeric(pts_df[rw,2])),
                    dstart = "01/01/2020", dfinish = "31/12/2020",
                    dem.res=1000, # requested resolution of the DEM from elevatr in m
                    DEP = c(0, 2.5,  5,  10,  15,  20,  30,  50,  100,  200), #cm
                    minshade = 0, maxshade = 90, runshade=1,
                    write_input=0, #save=1,
                    soilgrids=1, #CHECK WHY THIS ISN'T WORKING
                    spatial='../ncep_data', #NEED TO LOAD THIS DATA UP
                    Usrhyt = 0.01) # local height for organism
  return(m1$metout[1,])
  #for(spp in test_spp){
   # if(grepl(spp, se_pts_df[rw,4])){
    #  ecto_out<-bind_rows(ecto_out,
     #                     Te_est(spp) %>% mutate(rowid=rw))
    #print(paste('done with ',spp,' point', rw, 'of', nrow(se_pts_df)))
    }
  }
}

results <- mclapply(1:4,function(i)  anuranMNMs(), mc.cores = 2) # outputs list from each rep of function I think
save_this <- sapply(results,"[")
# save the outputs of this code
write.csv(save_this, file = paste0(PATH_out, "_", Sys.Date(), ".csv"), row.names=F)